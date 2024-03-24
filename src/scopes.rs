use core::hash::Hash;
use rustc_hash::FxHashMap;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Scopes<K, V> {
    base: FxHashMap<K, V>,
    scopes: Vec<FxHashMap<K, V>>,
}

impl<K: Eq + Hash, V: Eq + Hash> Scopes<K, V> {
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
}

pub trait ClosestStrKey {
    fn closest_str_key(&self, name: &str) -> Option<(&str, usize)>;
}

impl<V> ClosestStrKey for Scopes<&str, V> {
    fn closest_str_key(&self, name: &str) -> Option<(&str, usize)> {
        self.scopes
            .iter()
            .rev()
            .chain(core::iter::once(&self.base))
            .flat_map(|map| map.keys())
            .map(|a| (a, levenshtein::levenshtein(a, name)))
            .min_by_key(|(_, dist)| *dist)
            .map(|(a, dist)| (*a, dist))
    }
}

impl<V, S> ClosestStrKey for HashMap<&str, V, S> {
    fn closest_str_key(&self, name: &str) -> Option<(&str, usize)> {
        self.keys()
            .map(|a| (a, levenshtein::levenshtein(a, name)))
            .min_by_key(|(_, dist)| *dist)
            .map(|(a, dist)| (*a, dist))
    }
}
