use chumsky::prelude::*;
use std::path::Path;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Span<'file>(SimpleSpan<usize, &'file Path>);

impl<'file> Span<'file> {
    #[must_use]
    pub fn new(context: &'file Path, range: std::ops::Range<usize>) -> Span<'file> {
        Span(SimpleSpan::<usize, &'file Path>::new(context, range))
    }

    #[must_use]
    pub fn union(self, other: Span<'file>) -> Span<'file> {
        Span(self.0.union(other.0))
    }
}

impl<'file> chumsky::span::Span for Span<'file> {
    type Context = &'file Path;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Span::new(context, range)
    }

    fn context(&self) -> Self::Context {
        self.0.context()
    }

    fn start(&self) -> Self::Offset {
        self.0.start()
    }

    fn end(&self) -> Self::Offset {
        self.0.end()
    }
}

impl<'file> ariadne::Span for Span<'file> {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        self.0.context()
    }

    fn start(&self) -> usize {
        self.0.start()
    }

    fn end(&self) -> usize {
        self.0.end()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Spanned<'file, T>(pub T, pub Span<'file>);

impl<T> AsRef<T> for Spanned<'_, T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Spanned<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}
