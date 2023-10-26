//! A stack that is generally stored on the stack.
//!
//! The cursor type is intended to be used a lot, and we would prefer not to
//! allocate. Unfortunately, we do not know the maximum depth of a tree, but we
//! *do* know that they are not generally very deep.
//!
//! This stack type has a fixed-length buffer that is stored on the stack. If
//! this is inadequate, it fallsback to using a `Vec`.

use std::mem::MaybeUninit;

// the impl using `MaybeUninit` is adapted from the array_vec crate. We could
// use some external crate, but our needs are very modest and this is not much
// code.

/// Store a stack of objects, only allocating if depth 'N' is exceeded.
pub struct Stack<T, const N: usize> {
    parents: [MaybeUninit<T>; N],
    len: usize,
    fallback: Vec<T>,
}

impl<T, const N: usize> Stack<T, N> {
    // we need a const for array shorthand to work in the default() fn
    const INIT: MaybeUninit<T> = MaybeUninit::uninit();

    #[allow(dead_code)] // only used when debugging
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn push(&mut self, item: T) {
        if self.len < N {
            // items beyond len are always uninit
            unsafe { self.parents[self.len].as_mut_ptr().write(item) };
        } else {
            self.fallback.push(item);
        }
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        let parent = match self.len {
            0 => None,
            i if i <= N => {
                let parent = std::mem::replace(&mut self.parents[i - 1], MaybeUninit::uninit());
                // items below `len` must have been written
                Some(unsafe { parent.assume_init() })
            }
            _ => self.fallback.pop(),
        };
        self.len = self.len.saturating_sub(1);
        parent
    }
}

impl<T, const N: usize> Default for Stack<T, N> {
    fn default() -> Self {
        Self {
            parents: [Self::INIT; N],
            len: 0,
            fallback: Vec::new(),
        }
    }
}
