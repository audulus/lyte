use crate::*;
use std::cmp::Ordering;
use superslice::Ext;

/// Table of top level declarations.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DeclTable {

    /// All declarations, sorted by name.
    pub decls: Vec<Decl>,

    /// For quickly finding enums which contain a case.
    /// This is for resolving .enum_case expressions.
    pub enum_cases: Vec<(Name, usize)>,
}

/// Comparison function to sort declarations by name.
fn decl_cmp(a: &Decl, b: &Decl) -> Ordering {
    a.name().cmp(&b.name())
}

impl DeclTable {
    pub fn new(mut decls: Vec<Decl>) -> Self {
        decls.sort_by(decl_cmp);

        let mut enum_cases = vec![];

        let mut i = 0;
        for decl in &decls {
            if let Decl::Enum { cases, .. } = decl {
                for case in cases {
                    enum_cases.push( (*case, i ) )
                }
            }
            i += 1;
        }

        enum_cases.sort();

        Self { decls, enum_cases }
    }

    /// Returns a slice of all decls which match name.
    pub fn find(&self, name: Name) -> &[Decl] {
        let range = self.decls.equal_range_by(|x| x.name().cmp(&name));
        &self.decls[range]
    }

    /// Calls f for every enum containing a case named name.
    /// This is for resolving .enum_case expressions.
    pub fn find_enum(&self, name: Name, f: &mut impl FnMut(Name)) {
        let range = self.enum_cases.equal_range_by(|x| x.0.cmp(&name));
        for i in range {
            if let Decl::Enum { name, .. } = self.decls[i] {
                f(name)
            }
        }
    }

    /// Returns all types for a declaration name.
    pub fn types(&self, name: Name) -> Vec<TypeID> {
        let sl = self.find(name);
        let mut alts = vec![];

        for d in sl {
            match d {
                Decl::Func(_) => {
                    alts.push(d.ty());
                }
                Decl::Global{ .. } => {
                    alts.push(d.ty());
                }
                _ => ()
            }
        }

        alts
    }

    /// Returns all alternatives for a declaration name.
    pub fn alts(&self, name: Name) -> Vec<Alt> {
        let sl = self.find(name);
        let mut alts = vec![];

        for d in sl {
            match d {
                Decl::Func(FuncDecl { constraints, .. }) => {

                    let mut interfaces = vec![];
                    for c in constraints {
                        interfaces.push(
                            AltInterface{
                                interface: c.interface_name,
                                typevars: c.typevars.iter().map(|name| typevar(&*name)).collect()
                            }
                        )
                    }

                    alts.push(Alt{ty: d.ty(), interfaces} );
                }
                Decl::Global{ .. } => {
                    alts.push(Alt{ty: d.ty(), interfaces: vec![]});
                }
                _ => ()
            }
        }

        alts
    }

}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_sorted_decls() {
        let decls = vec![
            Decl::Global { name: Name::new("a".into()), ty: mk_type(Type::Void) },
            Decl::Global { name: Name::new("b".into()), ty: mk_type(Type::Void) },
            Decl::Global { name: Name::new("b".into()), ty: mk_type(Type::Void) },
            Decl::Global { name: Name::new("c".into()), ty: mk_type(Type::Void) },
        ];

        let sorted = DeclTable::new(decls);
        let d = sorted.find(Name::new("z".into()));
        assert_eq!(d.len(), 0);

        let d = sorted.find(Name::new("b".into()));
        assert_eq!(d.len(), 2);
        assert_eq!(d[0].name(), Name::new("b".into()));
        assert_eq!(d[1].name(), Name::new("b".into()));
    }
}
