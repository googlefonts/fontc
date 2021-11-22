use std::{ops::Range, sync::Mutex};

use fea_rs::{Kind, Source};
use lspower::lsp::{
    Diagnostic, DiagnosticSeverity, Position, Range as UghRange, SemanticToken, SemanticTokenType,
    SemanticTokens,
};

#[derive(Debug, Clone, Default)]
struct DocumentInner {
    text: String,
    offsets: Vec<usize>,
    tokens: Vec<(Kind, Range<usize>)>,
    errors: Vec<fea_rs::Diagnostic>,
}

#[derive(Debug, Default)]
pub(crate) struct Document {
    inner: Mutex<DocumentInner>,
}

impl Document {
    pub fn set_text(&self, text: String) {
        let offsets = compute_offsets(&text);
        let (tokens, errors) = parse(&text);
        let mut inner = self.inner.lock().unwrap();
        inner.text = text;
        inner.offsets = offsets;
        inner.tokens = tokens;
        inner.errors = errors;
    }

    pub fn replace_range(&self, range: Option<UghRange>, text: String) {
        let range = match range {
            Some(r) => r,
            None => return self.set_text(text),
        };
        let mut inner = self.inner.lock().unwrap();
        let range = from_lsp_range(range, &inner.offsets);
        inner.text.replace_range(range, &text);
        inner.offsets = compute_offsets(&inner.text);
        let (tokens, errors) = parse(&inner.text);
        inner.tokens = tokens;
        inner.errors = errors;
    }

    pub fn semantic_tokens(&self) -> SemanticTokens {
        let mut tokens = self.make_semantic_tokens();
        make_relative(&mut tokens);
        SemanticTokens {
            result_id: None,
            data: tokens,
        }
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let inner = self.inner.lock().unwrap();
        let mut result = Vec::new();
        for err in &inner.errors {
            let range = to_lsp_range(err.span(), &inner.offsets);
            result.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::Error),
                message: err.text().to_owned(),
                ..Default::default()
            })
        }
        result
    }

    #[cfg(test)]
    fn text_for_abs_token(&self, token: &SemanticToken) -> String {
        let inner = self.inner.lock().unwrap();
        let line_start = inner.offsets[token.delta_line as usize];
        let start = line_start + (token.delta_start as usize);
        let end = start + (token.length) as usize;
        let range = start..end;
        inner.text[range].to_owned()
    }

    fn make_semantic_tokens(&self) -> Vec<SemanticToken> {
        let inner = self.inner.lock().unwrap();

        let mut result: Vec<SemanticToken> = Vec::new();
        for (token_type, range) in inner
            .tokens
            .iter()
            .filter_map(|(kind, range)| style_for_kind(*kind).map(|style| (style, range)))
        {
            let start_pos = to_lsp_pos(range.start, &inner.offsets);
            result.push(SemanticToken {
                delta_line: start_pos.line,
                delta_start: start_pos.character,
                length: range.len() as u32,
                token_type,
                token_modifiers_bitset: 0,
            });
        }
        result
    }
}

fn make_relative(tokens: &mut [SemanticToken]) {
    let mut prev_line = 0;
    let mut prev_pos = 0;

    for token in tokens {
        let abs_line = token.delta_line;
        let abs_start = token.delta_start;
        let delta_line = abs_line - prev_line;
        let delta_start = if delta_line == 0 {
            token.delta_start - prev_pos
        } else {
            token.delta_start
        };
        prev_line = abs_line;
        prev_pos = abs_start;
        token.delta_line = delta_line;
        token.delta_start = delta_start;
    }
}

fn from_lsp_range(ugh: UghRange, offsets: &[usize]) -> Range<usize> {
    let start = from_lsp_pos(ugh.start, offsets);
    let end = from_lsp_pos(ugh.end, offsets);
    start..end
}

fn to_lsp_range(range: Range<usize>, offsets: &[usize]) -> UghRange {
    let start = to_lsp_pos(range.start, offsets);
    let end = to_lsp_pos(range.end, offsets);
    UghRange { start, end }
}

fn to_lsp_pos(offset: usize, offsets: &[usize]) -> Position {
    let line = offsets
        .iter()
        .position(|off| *off > offset)
        .unwrap_or_else(|| offsets.len())
        - 1;
    let line_start = offsets[line];
    let line_pos = offset - line_start;
    Position {
        line: line as u32,
        character: line_pos as u32,
    }
}

fn from_lsp_pos(pos: Position, offsets: &[usize]) -> usize {
    let line_offset = offsets[pos.line as usize];
    line_offset + (pos.character) as usize
}

/// the positions of the *start* of lines in the text
fn compute_offsets(text: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(
            text.bytes()
                .enumerate()
                .filter(|(_, b)| b == &b'\n')
                .map(|(i, _)| i + 1),
        )
        .collect()
}

fn parse(text: &str) -> (Vec<(Kind, Range<usize>)>, Vec<fea_rs::Diagnostic>) {
    let src = Source::from_str(text);
    let (root, errors, _) = fea_rs::parse_src(&src, None);
    let result = root.iter_tokens().map(|t| (t.kind, t.range())).collect();
    (result, errors)
}

pub static STYLES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD, // 0
    SemanticTokenType::NUMBER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::CLASS,
    SemanticTokenType::STRUCT, // 5
    SemanticTokenType::OPERATOR,
    SemanticTokenType::MODIFIER,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::ENUM_MEMBER, // 10
    SemanticTokenType::ENUM,
];

fn style_for_kind(kind: Kind) -> Option<u32> {
    let raw = match kind {
        Kind::Comment => 2,
        Kind::Number | Kind::Metric | Kind::Octal | Kind::Hex | Kind::Float => 1,
        Kind::String => 3,

        Kind::GlyphName | Kind::Cid => 10,
        Kind::Ident | Kind::Tag | Kind::Label => 5,
        Kind::TableKw
        | Kind::IncludeKw
        | Kind::LookupKw
        | Kind::LanguagesystemKw
        | Kind::AnchorDefKw
        | Kind::FeatureKw
        | Kind::MarkClassKw
        | Kind::AnonKw
        | Kind::LookupflagKw
        | Kind::ScriptKw
        | Kind::LanguageKw
        | Kind::GlyphClassDefKw => 0,
        Kind::NamedGlyphClass => 11,
        Kind::Backslash | Kind::Eq => 6,
        Kind::SubKw
        | Kind::PosKw
        | Kind::IgnoreKw
        | Kind::EnumKw
        | Kind::RsubKw
        | Kind::ByKw
        | Kind::FromKw => 9,
        _ => 700,
    };
    if raw > 20 {
        None
    } else {
        Some(raw)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offsets() {
        let fea = "lookup hi {\n\
pos a b 5;
}";
        let offsets = compute_offsets(&fea);
        assert_eq!(offsets, vec![0, 12, 23]);
    }

    #[test]
    fn token_ville() {
        let fea = "lookup hihi {\n\
pos a b 5;
} hihi;";

        let document = Document::default();
        document.set_text(fea.into());
        let tokens = document.make_semantic_tokens();
        assert_eq!(tokens.len(), 7);
        assert_eq!(document.text_for_abs_token(&tokens[0]), "lookup");
        assert_eq!(document.text_for_abs_token(&tokens[1]), "hihi");

        assert_eq!(tokens[2].delta_line, 1, "{:?}", tokens[2]);
        assert_eq!(tokens[2].delta_start, 0, "{:?}", tokens[2]);
        assert_eq!(document.text_for_abs_token(&tokens[2]), "pos");
        assert_eq!(tokens[3].delta_line, 1, "{:?}", tokens[3]);
        assert_eq!(tokens[3].delta_start, 4, "{:?}", tokens[3]);
        assert_eq!(document.text_for_abs_token(&tokens[3]), "a");
        assert_eq!(document.text_for_abs_token(&tokens[4]), "b");
        assert_eq!(document.text_for_abs_token(&tokens[5]), "5");

        assert_eq!(tokens[6].delta_line, 2, "{:?}", tokens[6]);
        assert_eq!(tokens[6].delta_start, 2, "{:?}", tokens[6]);
        assert_eq!(document.text_for_abs_token(&tokens[6]), "hihi");
    }

    #[test]
    fn token_ville_relative() {
        let fea = "lookup hi {\n\
pos a b 5;
} hi";

        let document = Document::default();
        document.set_text(fea.into());
        let tokens = document.semantic_tokens();
        let tokens = tokens.data;
        assert_eq!(tokens.len(), 7);
        assert_eq!(tokens[0].delta_line, 0);
        assert_eq!(tokens[0].delta_start, 0);

        assert_eq!(tokens[1].delta_line, 0);
        assert_eq!(tokens[1].delta_start, 7);

        assert_eq!(tokens[2].delta_line, 1, "{:?}", tokens[2]);
        assert_eq!(tokens[2].delta_start, 0);

        assert_eq!(tokens[3].delta_line, 0, "{:?}", tokens[3]);
        assert_eq!(tokens[3].delta_start, 4);

        assert_eq!(tokens[6].delta_line, 1, "{:?}", tokens[6]);
        assert_eq!(tokens[6].delta_start, 2);
    }
}
