use rustc_hash::FxHashMap;
use std::hash::Hash;

#[derive(Clone)]
pub struct Scopes<K, V> {
    base: FxHashMap<K, V>,
    scopes: Vec<FxHashMap<K, V>>,
}

impl<K: Eq + Hash, V> Scopes<K, V> {
    pub fn new() -> Self {
        Self {
            base: FxHashMap::default(),
            scopes: vec![],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.scopes
            .last_mut()
            .unwrap_or(&mut self.base)
            .insert(k, v);
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(k))
            .or_else(|| self.base.get(k))
    }

    pub fn contains_key(&self, k: &K) -> bool {
        self.scopes.iter().any(|scope| scope.contains_key(k)) || self.base.contains_key(k)
    }
}
