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
    pub fn into_box(self) -> Spanned<Box<T>> {
        Spanned(Box::new(self.0), self.1)
    }
}

impl<T> Spanned<Box<T>> {
    pub fn into_inner(self) -> Spanned<T> {
        Spanned(*self.0, self.1)
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Spanned<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}
