use chili_ast::ast;
use chili_ast::ty::TyKind;
use chili_ast::workspace::BindingInfoIdx;
use chili_error::DiagnosticResult;
use codespan_reporting::diagnostic::Diagnostic;
use core::fmt;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

trait Union {
    fn union(&self, other: &Self) -> Self;
}

// Implement union for HashMap such that the value in `self` is used over the value in `other` in
// the event of a collision.
impl<K, V> Union for HashMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    fn union(&self, other: &Self) -> Self {
        let mut res = self.clone();
        for (key, value) in other {
            res.entry(key.clone()).or_insert(value.clone());
        }
        res
    }
}

// A type variable represents a type that is not constrained
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct TyVar(pub(crate) u32);

impl fmt::Display for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl Into<TyKind> for TyVar {
    fn into(self) -> TyKind {
        TyKind::Var(self.0)
    }
}

impl TyVar {
    // Attempt to bind a type variable to a type, returning an appropriate substitution.
    fn bind(&self, ty: &TyKind) -> DiagnosticResult<Subst> {
        // Check for binding a variable to itself
        if let TyKind::Var(u) = ty {
            if *u == self.0 {
                return Ok(Subst::new());
            }
        }

        // The occurs check prevents illegal recursive types.
        if ty.ftv().contains(self) {
            return Err(
                Diagnostic::error().with_message(format!("occur check fails: {} vs {}", self, ty))
            );
        }

        let mut s = Subst::new();
        s.insert(self.clone(), ty.clone());
        Ok(s)
    }
}

// A source of unique type variables
pub(crate) struct TyVarGen {
    supply: u32,
}

impl TyVarGen {
    pub(crate) fn new() -> Self {
        Self { supply: 0 }
    }

    pub(crate) fn next(&mut self) -> TyVar {
        let v = TyVar(self.supply);
        self.supply += 1;
        v
    }
}

// A trait common to all things considered types.
pub(crate) trait Types {
    // Find the set of free variables in a type.
    fn ftv(&self) -> HashSet<TyVar>;
    // Apply a substitution to a type.
    fn apply(&self, subst: &Subst) -> Self;
}

impl<T: Types> Types for Vec<T> {
    // The free type variables of a vector of types is the union of the free type variables of each
    // of the types in the vector.
    fn ftv(&self) -> HashSet<TyVar> {
        self.iter()
            .map(|x| x.ftv())
            .fold(HashSet::new(), |set, x| set.union(&x).cloned().collect())
    }

    // To apply a substitution to a vector of types, just apply to each type in the vector.
    fn apply(&self, s: &Subst) -> Vec<T> {
        self.iter().map(|x| x.apply(s)).collect()
    }
}

impl Types for TyKind {
    fn ftv(&self) -> HashSet<TyVar> {
        // match self {
        //     // For a type variable, there is one free variable: the variable itself.
        //     &Type::Var(ref s) => [s.clone()].iter().cloned().collect(),

        //     // Primitive types have no free variables
        //     &Type::Int | &Type::Bool | &Type::Str => HashSet::new(),

        //     // For functions, we take the union of the free type variables of the input and output.
        //     &Type::Fun(ref i, ref o) => i.ftv().union(&o.ftv()).cloned().collect(),
        // }
        HashSet::new()
    }

    fn apply(&self, s: &Subst) -> TyKind {
        // match self {
        //     // If this type references a variable that is in the substitution, return it's
        //     // replacement type. Otherwise, return the existing type.
        //     &Type::Var(ref n) => s.get(n).cloned().unwrap_or(self.clone()),

        //     // To apply to a function, we simply apply to each of the input and output.
        //     &Type::Fun(ref t1, ref t2) => Type::Fun(Box::new(t1.apply(s)), Box::new(t2.apply(s))),

        //     // A primitive type is changed by a substitution.
        //     _ => self.clone(),
        // }
        self.clone()
    }
}

pub(crate) trait MostGeneralUnifier {
    fn most_general_unifier(&self, other: &Self) -> DiagnosticResult<Subst>;
}

impl MostGeneralUnifier for TyKind {
    // Most general unifier, a substitution S such that S(self) is congruent to S(other).
    fn most_general_unifier(&self, other: &Self) -> DiagnosticResult<Subst> {
        match (self, other) {
            // // For functions, we find the most general unifier for the inputs, apply the resulting
            // // substitution to the outputs, find the outputs' most general unifier, and finally
            // // compose the two resulting substitutions.
            // (&Type::Fun(ref in1, ref out1), &Type::Fun(ref in2, ref out2)) => {
            //     let sub1 = try!(in1.mgu(&*in2));
            //     let sub2 = try!(out1.apply(&sub1).mgu(&out2.apply(&sub1)));
            //     Ok(sub1.compose(&sub2))
            // }

            // // If one of the types is variable, we can bind the variable to the type.
            // // This also handles the case where they are both variables.
            // (&Type::Var(ref v), t) => v.bind(t),
            // (t, &Type::Var(ref v)) => v.bind(t),

            // // If they are both primitives, no substitution needs to be done.
            // (&Type::Int, &Type::Int) | (&Type::Bool, &Type::Bool) | (&Type::Str, &Type::Str) => {
            //     Ok(Subst::new())
            // }

            // // Otherwise, the types cannot be unified.
            // (t1, t2) => Err(Error::new(format!("types do not unify: {} vs {}", t1, t2))),
            _ => todo!(),
        }
    }
}

// A substitution is a mapping from type variables to types.
#[derive(Clone, Debug)]
pub(crate) struct Subst(HashMap<TyVar, TyKind>);

impl Deref for Subst {
    type Target = HashMap<TyVar, TyKind>;
    fn deref(&self) -> &HashMap<TyVar, TyKind> {
        &self.0
    }
}
impl DerefMut for Subst {
    fn deref_mut(&mut self) -> &mut HashMap<TyVar, TyKind> {
        &mut self.0
    }
}

impl Subst {
    // Construct an empty substitution.
    fn new() -> Subst {
        Subst(HashMap::new())
    }

    // To compose two substitutions, we apply self to each type in other and union the resulting
    // substitution with self.
    fn compose(&self, other: &Subst) -> Subst {
        Subst(
            self.union(
                &other
                    .iter()
                    .map(|(k, v)| (k.clone(), v.apply(self)))
                    .collect(),
            ),
        )
    }
}

// A polytype is a type in which there are a number of for-all quantifiers, i.e. some parts of the
// type may not be concrete but instead correct for all possible types.
#[derive(Clone, Debug)]
pub(crate) struct Polytype {
    pub vars: Vec<TyVar>,
    pub ty: TyKind,
}

impl Types for Polytype {
    // The free type variables in a polytype are those that are free in the internal type and not
    // bound by the variable mapping.
    fn ftv(&self) -> HashSet<TyVar> {
        self.ty
            .ftv()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    // Substitutions are applied to free type variables only.
    fn apply(&self, s: &Subst) -> Polytype {
        Polytype {
            vars: self.vars.clone(),
            ty: {
                let mut sub = s.clone();
                for var in &self.vars {
                    sub.remove(var);
                }
                self.ty.apply(&sub)
            },
        }
    }
}

impl Polytype {
    // Instantiates a polytype into a type. Replaces all bound type variables with fresh type
    // variables and return the resulting type.
    fn instantiate(&self, tvg: &mut TyVarGen) -> TyKind {
        let newvars = self.vars.iter().map(|_| TyKind::Var(tvg.next().0));
        self.ty
            .apply(&Subst(self.vars.iter().cloned().zip(newvars).collect()))
    }
}

// A type environment is a mapping from term variables to polytypes.
#[derive(Clone, Debug)]
pub(crate) struct TypeEnv(HashMap<BindingInfoIdx, Polytype>);

impl Deref for TypeEnv {
    type Target = HashMap<BindingInfoIdx, Polytype>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for TypeEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Types for TypeEnv {
    // The free type variables of a type environment is the union of the free type variables of
    // each polytype in the environment.
    fn ftv(&self) -> HashSet<TyVar> {
        self.values()
            .map(|x| x.clone())
            .collect::<Vec<Polytype>>()
            .ftv()
    }

    // To apply a substitution, we just apply it to each polytype in the type environment.
    fn apply(&self, s: &Subst) -> TypeEnv {
        TypeEnv(self.iter().map(|(k, v)| (k.clone(), v.apply(s))).collect())
    }
}

impl TypeEnv {
    // Construct an empty type environment.
    pub fn new() -> TypeEnv {
        TypeEnv(HashMap::new())
    }

    // Generalize creates a polytype
    fn generalize(&self, ty: &TyKind) -> Polytype {
        Polytype {
            vars: ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty: ty.clone(),
        }
    }

    // The meat of the type inference algorithm.
    fn ti(&self, exp: &ast::Expr, tvg: &mut TyVarGen) -> DiagnosticResult<(Subst, TyKind)> {
        let (s, t) = match *exp {
            // A variable is typed as an instantiation of the corresponding type in the
            // environment.
            // Exp::Var(ref v) => {
            //     match self.get(v) {
            //         Some(s) => (Subst::new(), s.instantiate(tvg)),
            //         None => Err(Error::new(format!("unbound variable: {}", v))),
            //     }
            // }

            // A literal is typed as it's primitive type.
            // Exp::Lit(ref l) => {
            //     (Subst::new(),
            //         match l {
            //         &Lit::Int(_) => Type::Int,
            //         &Lit::Bool(_) => Type::Bool,
            //         &Lit::Str(_) => Type::Str,
            //     })
            // }

            // An abstraction is typed by:
            // * Removing any existing type with the same name as the argument to prevent name
            // clashes.
            // * Inserting a new type variable for the argument.
            // * Inferring the type of the expression in the new environment to define the type of
            // the expression.
            // * Applying the resulting substitution to the argument to define the type of the
            // argument.
            // Exp::Abs(ref n, ref e) => {
            //     let tv = Type::Var(tvg.next());
            //     let mut env = self.clone();
            //     env.remove(n);
            //     env.insert(n.clone(),
            //                Polytype {
            //                    vars: Vec::new(),
            //                    ty: tv.clone(),
            //                });
            //     let (s1, t1) = try!(env.ti(e, tvg));
            //     Ok((s1.clone(), Type::Fun(Box::new(tv.apply(&s1)), Box::new(t1))))
            // }

            // An application is typed by:
            // * Inferring the type of the callee.
            // * Applying the resulting substitution to the argument and inferring it's type.
            // * Finding the most general unifier for the callee type and a function from the
            // argument type to a new type variable. This combines the previously known type of the
            // function and the type as it is now being used.
            // * Applying the unifier to the new type variable.
            // Exp::App(ref e1, ref e2) => {
            //     let (s1, t1) = try!(self.ti(e1, tvg));
            //     let (s2, t2) = try!(self.apply(&s1).ti(e2, tvg));
            //     let tv = Type::Var(tvg.next());
            //     let s3 = try!(t1.apply(&s2).mgu(&Type::Fun(Box::new(t2), Box::new(tv.clone()))));
            //     Ok((s3.compose(&s2.compose(&s1)), tv.apply(&s3)))
            // }

            // Let (variable binding) is typed by:
            // * Removing any existing type with the same name as the binding variable to prevent
            // name clashes.
            // * Inferring the type of the binding.
            // * Applying the resulting substitution to the environment and generalizing to the
            // binding type.
            // * Inserting the generalized type to the binding variable in the new environment.
            // * Applying the substution for the binding to the environment and inferring the type
            // of the expression.
            // Exp::Let(ref x, ref e1, ref e2) => {
            //     let mut env = self.clone();
            //     env.remove(x);
            //     let (s1, t1) = try!(self.ti(e1, tvg));
            //     let tp = env.apply(&s1).generalize(&t1);
            //     env.insert(x.clone(), tp);
            //     let (s2, t2) = try!(env.apply(&s1).ti(e2, tvg));
            //     Ok((s2.compose(&s1), t2))
            // }
            _ => todo!(),
        };

        //println!("{} :: {}", exp, t.apply(&s));

        Ok((s, t))
    }

    // Perform type inference on an expression and return the resulting type, if any.
    pub fn type_inference(&self, exp: &ast::Expr, tvg: &mut TyVarGen) -> DiagnosticResult<()> {
        let (s, t) = self.ti(exp, tvg)?;
        let t2 = t.apply(&s);
        Ok(())
    }
}
