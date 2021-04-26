use super::structure::{Age, Categories, Category, SName, Structure};
use maplit::hashset;
use serde::ser::{Serialize, SerializeStruct, Serializer};
use std::collections::{HashMap, HashSet};

#[derive(Default, Debug, PartialEq)]
pub struct StructureBuilder {
    pub ages_can_build_free_in: HashSet<Age>,
    pub built_structures: HashMap<Category, HashSet<SName<'static>>>,
}

impl StructureBuilder {
    pub fn apply_construct_free_once_per_age(&mut self, current_age: Age) {
        self.ages_can_build_free_in
            .extend(current_age.get_current_and_future_ages());
    }
    pub fn already_built<T>(&self, structure: &'static Structure<'static, T>) -> bool {
        if let Some(structures) = self.built_structures.get(&structure.category()) {
            structures.contains(structure.name())
        } else {
            false
        }
    }
    pub fn can_build_structure_from_dependencies<T>(
        &self,
        structure: &'static Structure<'static, T>,
    ) -> bool {
        let all_structure_names_built: HashSet<SName<'static>> =
            self.built_structures.values().cloned().flatten().collect();
        for dependency in structure.dependencies() {
            if all_structure_names_built.contains(dependency) {
                return true;
            }
        }
        false
    }
    pub fn can_build_structure_for_free<T>(
        &self,
        _structure: &'static Structure<'static, T>,
        current_age: Age,
    ) -> bool {
        self.ages_can_build_free_in.contains(&current_age)
    }
    pub fn build_structure<T>(&mut self, structure: &'static Structure<'static, T>) {
        if let Some(structures) = self.built_structures.get_mut(&structure.category()) {
            structures.insert(structure.name());
        } else {
            self.built_structures
                .insert(structure.category(), hashset![structure.name()]);
        }
    }
    pub fn count_structures_for_categories(&self, categories: &Categories) -> usize {
        self.built_structures
            .iter()
            .map(|(category, structures)| {
                if categories.contains(&category) {
                    structures.len()
                } else {
                    0
                }
            })
            .sum::<usize>()
    }
    pub fn apply_used_construct_for_free_for_age(&mut self, age: &Age) {
        self.ages_can_build_free_in.remove(age);
    }
}

impl Serialize for StructureBuilder {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("StructureBuilder", 3)?;
        s.serialize_field("type", "StructureBuilder")?;
        s.serialize_field("ages_can_build_free_in", &self.ages_can_build_free_in)?;
        s.serialize_field("built_structures", &self.built_structures)?;
        s.end()
    }
}
