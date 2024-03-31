use chumsky::prelude::*;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Ctx(pub usize);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Span(SimpleSpan<usize, Ctx>);

impl Span {
    #[must_use]
    pub fn new(context: Ctx, range: core::ops::Range<usize>) -> Self {
        Self(SimpleSpan::<usize, Ctx>::new(context, range))
    }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        Self(self.0.union(other.0))
    }
}

impl chumsky::span::Span for Span {
    type Context = Ctx;

    type Offset = usize;

    fn new(context: Self::Context, range: core::ops::Range<Self::Offset>) -> Self {
        Self::new(context, range)
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned(&self.0, self.1)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned(f(self.0), self.1)
    }

    pub fn map_with_span<U>(self, f: impl FnOnce(T, Span) -> U) -> Spanned<U> {
        Spanned(f(self.0, self.1), self.1)
    }

    pub fn boxed(self) -> Spanned<Box<T>> {
        self.map(Box::new)
    }
}

impl<T> Spanned<Box<T>> {
    pub fn into_inner(self) -> Spanned<T> {
        Spanned(*self.0, self.1)
    }
}
