// https://www.codewars.com/kata/isomorphism/rust

#![allow(dead_code)]

/// so, when are two type, `a` and `b`, considered equal?
/// a definition might be, it is possible to go from `a` to `b`,
/// and from `b` to `a`.
/// Going a roundway trip should leave you the same value.
/// Unfortunately it is virtually impossible to test this.
/// This is called ISO.
pub enum Void { }

impl PartialEq for Void {
    fn eq(&self, _: &Void) -> bool {
        true
    }
}

pub fn absurd(_: Void) -> ! {
    panic!("You must be kidding! Where did you find that void instance?");
}

pub type ISO<A: 'static, B: 'static> = (Box<Fn(A) -> B>, Box<Fn(B) -> A>);

pub fn iso<A: 'static, B: 'static, F1, F2>(a: F1, b: F2) -> ISO<A, B>
    where F1: 'static + Fn(A) -> B,
          F2: 'static + Fn(B) -> A,
{
    (Box::new(a), Box::new(b))
}

/// given ISO a b, we can go from a to b
pub fn sub_st_l<A, B>(iso: ISO<A, B>) -> Box<Fn(A) -> B> { iso.0 }

/// and vice versa
pub fn sub_st_r<A, B>(iso: ISO<A, B>) -> Box<Fn(B) -> A> { iso.1 }

/// There can be more than one ISO a b
pub fn iso_bool() -> ISO<bool, bool> {
    iso(|x: bool| x, |x: bool| x)
}

pub fn iso_bool_not() -> ISO<bool, bool> {
    iso(|x: bool| !x, |x: bool| !x)
}

/// isomorphism is reflexive
pub fn refl<A: 'static>() -> ISO<A, A> {
    iso(|x| x, |x| x)
}

/// isomorphism is symmetric
pub fn symm<A: 'static, B: 'static>(i: ISO<A, B>) -> ISO<B, A> {
    (i.1, i.0)
}

/// isomorphism is transitive
pub fn trans<A: 'static, B: 'static, C: 'static>(
    ab: ISO<A, B>, bc: ISO<B, C>
) -> ISO<A, C> {
    let ab0 = ab.0;
    let ab1 = ab.1;
    let bc0 = bc.0;
    let bc1 = bc.1;
    iso(
        move |a: A| bc0(ab0(a)),
        move |c: C| ab1(bc1(c)),
    )
}

/// we can combine isomorphism
pub fn iso_tuple<A: 'static, B: 'static, C: 'static, D: 'static>(
    ab: ISO<A, B>, cd: ISO<C, D>
) -> ISO<(A, C), (B, D)> {
    let ab0 = ab.0;
    let ab1 = ab.1;
    let cd0 = cd.0;
    let cd1 = cd.1;
    iso(
        move |(a, c)| (ab0(a), cd0(c)),
        move |(b, d)| (ab1(b), cd1(d)),
    )
}

pub fn iso_vec<A: 'static, B: 'static>(i: ISO<A, B>) -> ISO<Vec<A>, Vec<B>> {
    let i0 = i.0;
    let i1 = i.1;
    iso(
        move |v: Vec<A>| v.into_iter().map(|a: A| i0(a)).collect(),
        move |v: Vec<B>| v.into_iter().map(|b: B| i1(b)).collect(),
    )
}

pub fn iso_option<A: 'static, B: 'static>(
    i: ISO<A, B>
) -> ISO<Option<A>, Option<B>> {
    let i0 = i.0;
    let i1 = i.1;
    iso(
        move |x: Option<A>| x.map(|a| i0(a)),
        move |x: Option<B>| x.map(|b| i1(b)),
    )
}

pub fn iso_result<A: 'static, B: 'static, C: 'static, D: 'static>(
    ab: ISO<A, B>, cd: ISO<C, D>
) -> ISO<Result<A, C>, Result<B, D>> {
    let ab0 = ab.0;
    let ab1 = ab.1;
    let cd0 = cd.0;
    let cd1 = cd.1;
    iso(
        move |x: Result<A, C>| x.map(|a| ab0(a)).map_err(|c| cd0(c)),
        move |x: Result<B, D>| x.map(|b| ab1(b)).map_err(|d| cd1(d)),
    )
}

/// Going another way is hard (and is generally impossible)
/// Remember, for all valid ISO, converting and converting back
/// is the same as the original value.
/// You need this to prove some case are impossible.
pub fn iso_un_option<A: 'static, B: 'static>(
    i: ISO<Option<A>, Option<B>>
) -> ISO<A, B> {
    let i0 = i.0;
    let i1 = i.1;
    iso(
        move |a| i0(Some(a)).unwrap_or_else(|| i0(None).unwrap()),
        move |b| i1(Some(b)).unwrap_or_else(|| i1(None).unwrap()),
    )
}

/// inf + 0 = inf + 1
pub fn iso_eu() -> ISO<Result<Vec<()>, ()>, Result<Vec<()>, Void>> {
    iso(
        |x: Result<Vec<()>, ()>| match x {
            Ok(v) => Ok(v.into_iter().chain(vec![()]).collect()),
            Err(e) => Ok(vec![]),
        },
        |x: Result<Vec<()>, Void>| match x {
            Ok(v) => if v.is_empty() {
                Err(())
            } else {
                Ok(v.into_iter().skip(1).collect())
            },
            Err(e) => Err(absurd(e)),
        },
    )
}

pub type IsoFL<A, B, C, D> = Box<FnOnce(Box<Fn(A) -> C>) -> Box<FnOnce(B) -> D>>;
pub type IsoFR<A, B, C, D> = Box<FnOnce(Box<Fn(B) -> D>) -> Box<FnOnce(A) -> C>>;
pub type IsoF<A, B, C, D> = (IsoFL<A, B, C, D>, IsoFR<A, B, C, D>);

/// translator note:
/// FnBox is not yet supported, we can only return an uncallable
/// Box<FnOnce> (RetFunc). You should return the function with
/// correct type, which will be checked by the tests.
/// The type annotation is shown above. You need you return something like
/// (Box::new(...), Box::new(...))
pub fn iso_func<A: 'static, B: 'static, C: 'static, D: 'static>(
    _ab: ISO<A, B>, _cd: ISO<C, D>
) -> IsoF<A, B, C, D> {
    (
        Box::new(|_ac: Box<Fn(A) -> C>| unimplemented!()),
        Box::new(|_bd: Box<Fn(B) -> D>| unimplemented!()),
    )
    // iso(
    //     |ac: Fn(A) -> C| (|b: B| cd.0(ac(ab.1(b)))),
    //     |bd: Fn(B) -> D| (|a: A| cd.1(bd(ab.0(a)))),
    // )
}

/// And we have isomorphism on isomorphism!
pub fn iso_symm<A: 'static, B: 'static>() -> ISO<ISO<A, B>, ISO<B, A>> {
    (
        Box::new(|ab: ISO<A, B>| iso(
            { let ab1 = ab.1; move |x| ab1(x) },
            { let ab0 = ab.0; move |x| ab0(x) },
        )),
        Box::new(|ba: ISO<B, A>| iso(
            { let ba1 = ba.1; move |x| ba1(x) },
            { let ba0 = ba.0; move |x| ba0(x) },
        )),
    )
    // iso(
    //     |ab: ISO<A, B>| iso(|x| ab.1(x), |x| ab.0(x)),
    //     |ba: ISO<B, A>| iso(|x| ba.1(x), |x| ba.0(x)),
    // )
}
