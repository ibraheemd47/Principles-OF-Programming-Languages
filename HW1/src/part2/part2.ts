import * as R from "ramda";

const stringToArray = R.split("");

/* Question 1 */
const vowels: string[] = ['a', 'e', 'i', 'o', 'u'];
export const countVowels= (str: string ):number => {
    return filteredArray(stringToArray(str.toLowerCase())).length;
    
}
export const filteredArray = (str:string[]):string[] => {
    return(str.filter(t => vowels.includes(t)));
}


/* Question 2 */
const digitslowercaseLetters = "abcdefghijklmnopqrstuvwxyz0123456789".split("");
const punctuation = ".,!?;:'\"-()[]{}".split("");

export const isPalindrome = (str:string):boolean =>{
    return checkPalindrome(filteredstring(str));
    
}

export const filteredstring = (str:string):string[] => {
    return(stringToArray(str.toLowerCase()).filter(t => digitslowercaseLetters.includes(t) ));
}
const checkPalindrome = (arr: string[]): boolean =>
    arr.length <= 1
      ? true
      : R.head(arr) === R.last(arr) && checkPalindrome(R.init(R.tail(arr)));

/* Question 3 */
export type WordTree = {
    root: string;
    children: WordTree[];
}

export const treeToSentence = (tree: WordTree): string =>
    [tree.root, ...tree.children.map(treeToSentence)].join(" ");

