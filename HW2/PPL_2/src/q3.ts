import { AppExp, Exp, isCExp, PrimOp, Program } from './L3/L3-ast';
import { Result, makeFailure, makeOk} from './shared/result';
import { isProgram, isDefineExp, isNumExp, isBoolExp, isVarRef, isIfExp, isPrimOp,isAppExp, isProcExp, CExp, makeProgram, DefineExp} from './L3/L3-ast';

/*
Purpose: Transform L2 AST to JavaScript program string
Signature: l2ToJS(l2AST)
Type: [EXP | Program] => Result<string>
*/

 

export const l2ToJS = (exp: Exp | Program): Result<string> => 
isProgram(exp) ? l2ToJSProgram(exp) :
isDefineExp(exp) ? l2ToJSDefine(exp) :
isCExp(exp) ? l2ToJSCExp(exp) :
makeFailure("Unknown expression type");

const l2ToJSProgram = (exp: Program): Result<string> =>
makeOk(exp.exps.map(e => {
    const result = l2ToJS(e);
    return result.tag === "Ok" ? result.value : "";
}).join(";\n"));

const l2ToJSDefine = (exp: DefineExp): Result<string> => {
    const valResult = l2ToJSCExp(exp.val);
    return valResult.tag === "Ok" ? 
        makeOk(`const ${exp.var.var} = ${valResult.value}`) :
        valResult;
}

const l2ToJSCExp = (exp: CExp): Result<string> => 
isNumExp(exp) ? makeOk(exp.val.toString()) :
isBoolExp(exp) ? makeOk(exp.val ? "true" : "false") :
isVarRef(exp) ? makeOk(exp.var) :
isIfExp(exp) ? 
    ((exp): Result<string> => {
        const testResult = l2ToJSCExp(exp.test);
        const thenResult = l2ToJSCExp(exp.then);
        const altResult = l2ToJSCExp(exp.alt);
        return testResult.tag === "Ok" && thenResult.tag === "Ok" && altResult.tag === "Ok" ?
            makeOk(`(${testResult.value} ? ${thenResult.value} : ${altResult.value})`) :
            makeFailure("Failed to convert if expression");
    })(exp) :
isPrimOp(exp) ? makeOk(convertPrimOp(exp.op)) :
isProcExp(exp) ? 
    ((exp): Result<string> => {
        const bodyResult = l2ToJSCExp(exp.body[0]);
        return bodyResult.tag === "Ok" ?
            makeOk(`((${exp.args.map(p => p.var).join(",")}) => ${bodyResult.value})`) :
            bodyResult;
    })(exp) :
isAppExp(exp) ? 
isPrimOp(exp.rator) ? makeOk(handlePrimOpApp(exp)) :
(() => {
    const ratorResult = l2ToJSCExp(exp.rator);
    const randsResults = exp.rands.map(l2ToJSCExp);
    const randsValues = randsResults.map(r => r.tag === "Ok" ? r.value : "");
    return ratorResult.tag === "Ok" && randsResults.every(r => r.tag === "Ok") ?
        makeOk(`${ratorResult.value}(${randsValues.join(",")})`) :
        makeFailure("Failed to convert application expression");
})() :
makeFailure("Unknown expression type");

const convertPrimOp = (op: string): string =>
op === "=" ? "===" :
op === "eq?" ? "===" :
op === "number?" ? "((x) => typeof(x) === 'number')" :
op === "boolean?" ? "((x) => typeof(x) === 'boolean')" :
op;

const handlePrimOpApp = (exp: AppExp): string => {
const op = (exp.rator as PrimOp).op;
const args = exp.rands.map(r => {
    const result = l2ToJSCExp(r);
    return result.tag === "Ok" ? result.value : "";
});
return op === "and" ? `(${args.join(" && ")})` :
   op === "or" ? `(${args.join(" || ")})` :
   op === "not" ? `(!${args[0]})` :
   op === "=" || op === "eq?" ? `(${args.join(" === ")})` :
   op === "number?" ? `((x) => typeof(x) === 'number')(${args[0]})` :
   op === "boolean?" ? `((x) => typeof(x) === 'boolean')(${args[0]})` :
   `(${args.join(` ${op} `)})`;
};