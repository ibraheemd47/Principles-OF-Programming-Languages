import {
    makeProgram, Program, CExp, Exp,
    VarRef, LitExp, IfExp, ProcExp, LetExp, DictExp,
    makeAppExp, makeVarRef, makeLitExp,
    makeIfExp, makeProcExp, makeBinding, makeLetExp,
    makeBoolExp, makeVarDecl, makeDefineExp, makePrimOp
} from "./L32/L32-ast";
import { isDefineExp } from "./L32/L32-ast";
import { SExpValue, makeEmptySExp, makeCompoundSExp, makeSymbolSExp } from "./L32/L32-value";

const list4SExp = (elts: SExpValue[]) =>
    elts.reduceRight((tl, hd) => makeCompoundSExp(hd, tl), makeEmptySExp());

export function C2SExp(ce: CExp): SExpValue {

    return (ce.tag === "NumExp" || ce.tag === "BoolExp" || ce.tag === "StrExp" || ce.tag === "LitExp")
        ? ce.val : ce.tag === "VarRef" ? makeSymbolSExp(ce.var) : ce.tag === "PrimOp"
            ? makeSymbolSExp(ce.op)


            : ce.tag === "IfExp"
                ? list4SExp([
                    makeSymbolSExp("if"),
                    C2SExp(ce.test),
                    C2SExp(ce.then),
                    C2SExp(ce.alt),
                ])


                : ce.tag === "ProcExp"
                    ? list4SExp([
                        makeSymbolSExp("lambda"),
                        list4SExp(ce.args.map(a => makeSymbolSExp(a.var))),
                        ...ce.body.map(C2SExp),
                    ])


                    : ce.tag === "LetExp"
                        ? list4SExp([
                            makeSymbolSExp("let"),
                            list4SExp(
                                ce.bindings.map(b =>
                                    list4SExp([makeSymbolSExp(b.var.var), C2SExp(b.val)])
                                )
                            ),
                            ...ce.body.map(C2SExp),
                        ])


                        : ce.tag === "AppExp"
                            ? list4SExp([
                                C2SExp(ce.rator),
                                ...ce.rands.map(C2SExp),
                            ])


                            : ce.tag === "DictExp"
                                ? list4SExp([
                                    makeSymbolSExp("dict"),
                                    ...ce.pairs.map(p =>
                                        list4SExp([p.key, C2SExp(p.val)])
                                    ),
                                ])


                                : (ce as any);

}


const asLit = (v: CExp): CExp =>
    ["NumExp", "BoolExp", "StrExp", "LitExp"].includes(v.tag)
        ? v
        : makeLitExp(C2SExp(v));

export const Dict2App = (p: Program): Program => {
    function transform1(ce: CExp): CExp {
        return ce.tag === "IfExp"
            ? makeIfExp(
                transform1(ce.test),
                transform1(ce.then),
                transform1(ce.alt)
            )
            : ce.tag === "AppExp"
                ? makeAppExp(
                    transform1(ce.rator),
                    ce.rands.map(transform1)
                )
                : ce.tag === "ProcExp"
                    ? makeProcExp(
                        ce.args,
                        ce.body.map(transform1)
                    )
                    : ce.tag === "LetExp"
                        ? makeLetExp(
                            ce.bindings.map(b =>
                                makeBinding(b.var.var, transform1(b.val))
                            ),
                            ce.body.map(transform1)
                        )
                        : ce.tag === "DictExp"
                            ? makeAppExp(
                                makeVarRef("dict"),
                                [makeLitExp(
                                    list4SExp(
                                        ce.pairs.map(p =>
                                            makeCompoundSExp(p.key, C2SExp(p.val))
                                        )
                                    )
                                )
                                ]
                            )
                            : ce;
    }

    const transformExp1 = (e: Exp): Exp =>
        isDefineExp(e)
            ? makeDefineExp(e.var, transform1(e.val))
            : transform1(e as CExp);

    return makeProgram(p.exps.map(transformExp1));
};

export const L32toL3 = (p: Program): Program => {
    const chainIfs = (k: VarRef, pairs: { key: SExpValue, val: CExp }[]) =>
        pairs
            .slice().reverse()
            .reduce<CExp>(
                (alt, { key, val }) =>
                    makeIfExp(
                        makeAppExp(makePrimOp("eq?"), [k, makeLitExp(key)]),
                        asLit(val),
                        alt
                    ),
                makeBoolExp(false)
            );

    function transform2(ce: CExp): CExp {

        return ce.tag === "IfExp"
            ? makeIfExp(
                transform2(ce.test),
                transform2(ce.then),
                transform2(ce.alt)
            )
            : ce.tag === "AppExp"
                ? makeAppExp(
                    transform2(ce.rator),
                    ce.rands.map(transform2)
                )
                : ce.tag === "ProcExp"
                    ? makeProcExp(
                        ce.args,
                        ce.body.map(transform2)
                    )
                    : ce.tag === "LetExp"
                        ? makeLetExp(
                            ce.bindings.map(b =>
                                makeBinding(b.var.var, transform2(b.val))
                            ),
                            ce.body.map(transform2)
                        )
                        : ce.tag === "DictExp"
                            ? makeProcExp(
                                [makeVarDecl("k")],
                                [chainIfs(makeVarRef("k"), ce.pairs)]
                            )
                            : ce;
    }

    const transformExp2 = (e: Exp): Exp =>
        isDefineExp(e)
            ? makeDefineExp(e.var, transform2(e.val))
            : transform2(e as CExp);

    return makeProgram(p.exps.map(transformExp2));
};
