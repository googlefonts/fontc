//! Handling glyphsapp syntax extensions

use std::{
    collections::HashMap,
    ops::{AddAssign, DivAssign, MulAssign, SubAssign},
};

use fontdrasil::coords::NormalizedLocation;

use crate::typed;

/// The result of evaluting a glyphs number value expression
#[derive(Debug, PartialEq)]
pub enum ResolvedValue {
    Scalar(i16),
    Variable(HashMap<NormalizedLocation, i16>),
}

// used while we're working
#[derive(Debug)]
enum Value {
    Lit(f64),
    Named(HashMap<NormalizedLocation, f64>),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
}

impl Operator {
    fn priority(self) -> u8 {
        match self {
            Operator::Plus | Operator::Minus => 5,
            Operator::Mul | Operator::Div => 42,
        }
    }
}

impl Value {
    /// Apply the provided operation to `self` with the provided (rhs) argument.
    ///
    /// If the two sides are the same variant, the result is the same variant.
    /// If the two sides differ, the result is always a map (the 'Named' variant)
    fn apply(&mut self, mut rhs: Self, op: Operator) {
        // to simplify op logic, if two types are different ensure Named
        // is always the lhs
        if matches!((&self, &rhs), (Value::Lit(_), Value::Named(_))) {
            std::mem::swap(self, &mut rhs);
        }

        match op {
            Operator::Plus => self.do_op(rhs, f64::add_assign),
            Operator::Minus => self.do_op(rhs, f64::sub_assign),
            Operator::Mul => self.do_op(rhs, f64::mul_assign),
            Operator::Div => self.do_op(rhs, f64::div_assign),
        };
    }

    fn do_op(&mut self, rhs: Self, op: impl Fn(&mut f64, f64)) {
        match (self, rhs) {
            (Value::Lit(v1), Value::Lit(v2)) => op(v1, v2),
            (Value::Named(v1), Value::Named(v2)) => {
                for (k, v) in v1.iter_mut() {
                    op(v, v2.get(k).copied().unwrap_or_default());
                }
            }
            (Value::Named(v1), Value::Lit(v2)) => {
                v1.values_mut().for_each(|v| op(v, v2));
            }
            _ => unreachable!("normalized in apply"),
        }
    }
}

// a simple expression parser/resolver
//
// https://cp-algorithms.com/string/expression_parsing.html was used as a reference.
pub(crate) fn resolve_glyphs_app_expr(
    expr: &typed::GlyphsAppNumberExpr,
    mut var_info_fn: impl FnMut(&str) -> HashMap<NormalizedLocation, f64>,
) -> ResolvedValue {
    fn process_op(stack: &mut Vec<Value>, op: Operator) {
        let rhs = stack.pop().unwrap();
        stack.last_mut().unwrap().apply(rhs, op);
    }

    let (mut val_stack, mut op_stack) = (Vec::new(), Vec::<Operator>::new());
    for item in expr.items() {
        match item {
            typed::GlyphsAppExprItem::Ident(ident) => {
                let val = var_info_fn(ident.text());
                val_stack.push(Value::Named(val));
            }
            typed::GlyphsAppExprItem::Lit(lit) => val_stack.push(Value::Lit(lit.parse() as _)),
            // new operator: resolve any existing higher priority ops on the stack
            typed::GlyphsAppExprItem::Operator(op) => {
                let op = Operator::from(op);
                while op_stack.last().map(|x| x.priority()).unwrap_or_default() >= op.priority() {
                    process_op(&mut val_stack, op_stack.pop().unwrap());
                }
                op_stack.push(op);
            }
        }
    }

    // now resolve the remaining expression
    while let Some(op) = op_stack.pop() {
        process_op(&mut val_stack, op);
    }

    let result = match val_stack.pop() {
        // convert back to i16
        Some(Value::Named(val)) => {
            ResolvedValue::Variable(val.into_iter().map(|(k, v)| (k, v.round() as _)).collect())
        }
        Some(Value::Lit(val)) => ResolvedValue::Scalar(val.round() as _),
        _ => ResolvedValue::Scalar(0),
    };
    assert!(val_stack.is_empty());
    result
}

impl From<typed::GlyphsAppOperator> for Operator {
    fn from(src: typed::GlyphsAppOperator) -> Operator {
        match src {
            typed::GlyphsAppOperator::Plus(_) => Operator::Plus,
            typed::GlyphsAppOperator::Minus(_) => Operator::Minus,
            typed::GlyphsAppOperator::Mul(_) => Operator::Mul,
            typed::GlyphsAppOperator::Div(_) => Operator::Div,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::TokenSet;

    use self::typed::AstNode;

    use super::*;

    fn parse_expr(text: &str) -> typed::GlyphsAppNumberExpr {
        let node = crate::parse::parse_node(text, |p| {
            crate::parse::grammar::expect_glyphs_number_value(p, TokenSet::EMPTY);
        });
        let node = typed::GlyphsAppNumber::cast(&node.into()).unwrap();
        node.iter()
            .find_map(typed::GlyphsAppNumberExpr::cast)
            .unwrap()
    }

    fn simple_number_value() -> HashMap<NormalizedLocation, f64> {
        let loc1 = NormalizedLocation::for_pos(&[("wght", 0.1)]);
        let loc2 = NormalizedLocation::for_pos(&[("wght", 0.9)]);
        HashMap::from([(loc1, 10.), (loc2, 23.)])
    }

    // a little helper for more readable assertions
    fn ordered_eq<const N: usize>(lhs: &ResolvedValue, rhs: [i16; N]) -> bool {
        let ResolvedValue::Variable(lhs) = lhs else {
            return false;
        };
        let mut ordered: Vec<_> = lhs.iter().collect();
        ordered.sort_by_key(|(k, _)| *k);
        let ordered = ordered.iter().map(|(_, v)| **v).collect::<Vec<_>>();
        ordered == rhs
    }

    #[test]
    fn scalar_arithmetic() {
        let text = "${2 + 4 * 5 / 7 - 3}";

        let expr = parse_expr(text);
        let val = resolve_glyphs_app_expr(&expr, |_| Default::default());
        let expected = (2f64 + 4. * 5. / 7. - 3.).round() as i16;
        assert_eq!(val, ResolvedValue::Scalar(expected));
    }

    #[test]
    fn mixed_arithmetic() {
        let text = "${padding * 2}";
        let expr = parse_expr(text);
        let val: ResolvedValue = resolve_glyphs_app_expr(&expr, |_| simple_number_value());
        assert!(ordered_eq(&val, [20i16, 46]));
    }

    #[test]
    fn mixed_arithmetic_other_order() {
        let text = "${2 * padding}";
        let expr = parse_expr(text);
        let val: ResolvedValue = resolve_glyphs_app_expr(&expr, |_| simple_number_value());
        assert!(ordered_eq(&val, [20i16, 46]));
    }

    #[test]
    fn negative_numbers() {
        let text = "${2 - -3}";
        let expr = parse_expr(text);
        let val = resolve_glyphs_app_expr(&expr, |_| Default::default());
        assert_eq!(val, ResolvedValue::Scalar(2 + 3));
    }

    #[test]
    fn negative_parse_ambiguity() {
        let text = "${2-3}";
        let expr = parse_expr(text);
        let val = resolve_glyphs_app_expr(&expr, |_| Default::default());
        assert_eq!(val, ResolvedValue::Scalar(2 - 3));
    }

    #[test]
    fn float_math() {
        let text = "${padding/2.4}";
        let expr = parse_expr(text);
        let val: ResolvedValue = resolve_glyphs_app_expr(&expr, |_| simple_number_value());
        assert!(ordered_eq(&val, [4i16, 10]));
    }

    #[test]
    fn bare_values() {
        let expr = parse_expr("${padding}");
        let val: ResolvedValue = resolve_glyphs_app_expr(&expr, |_| simple_number_value());
        assert!(ordered_eq(&val, [10i16, 23]));

        let expr = parse_expr("${42}");
        let val = resolve_glyphs_app_expr(&expr, |_| Default::default());
        assert_eq!(val, ResolvedValue::Scalar(42));
    }
}
