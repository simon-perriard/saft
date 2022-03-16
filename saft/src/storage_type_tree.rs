use crate::analysis_utils::*;
use rustc_middle::ty::TyCtxt;

pub struct StorageTypeNode {
    name: String,
    children: Vec<StorageTypeNode>,
}

impl StorageTypeNode {
    pub fn new(name: String) -> Self {
        StorageTypeNode {
            name,
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, name: String) {
        let new_child = StorageTypeNode::new(name);
        self.children.push(new_child);
    }

    pub fn get_last_as_mut_ref(&mut self) -> &mut StorageTypeNode {
        self.children.last_mut().unwrap()
    }

    pub fn visit_display(&self, tcx: &TyCtxt) {
        print!("{:?}", self.name);
        if self.children.len() > 0 {
            print!("<");

            let mut count = self.children.len() - 1;
            for child in self.children.iter() {
                child.visit_display(tcx);
                if count > 0 {
                    print!(", ");
                }
                count -= 1;
            }

            print!(">");
        }
    }
}

pub struct StorageTypeTree {
    root: StorageTypeNode,
}

impl StorageTypeTree {
    pub fn new(root: StorageTypeNode) -> Self {
        StorageTypeTree { root }
    }

    pub fn visit_display(&self, tcx: &TyCtxt) {
        self.root.visit_display(tcx);
        println!("");
    }

    pub fn get_root_as_mut_ref(&mut self) -> &mut StorageTypeNode {
        &mut self.root
    }
}
