//Q1
export function all<T>(promises : Array<Promise<T>>) : Promise<Array<T>> {

  return new Promise<T[]>( (resolve, reject) => {
    //resolve([]);
    if (promises.length === 0) {
      resolve([]);
      return;
    }
    //TODO
    //create an arrray of resolve for the promises
    //if the array length is equal to the promises length, resolve the promise
    //if any of the promises is rejected, reject the promise
    const results: T[] = new Array(promises.length);
    let resolvedCount = 0;
    promises.forEach((promise, index) => {
      promise.then(value => {
        results[index] = value;
        
        resolvedCount++;
        if (resolvedCount === promises.length) {
          resolve(results);
        }
      }).catch(err => {
        reject(err);
      });
    });
  });
}

  
// Q2
export function* Fib1() {
	// @TODO
  let a = 1, b = 1;
  yield a; // first Fibonacci number
  yield b; // second Fibonacci number
  while (true) {
    const next = a + b;
    yield next; // yield the next Fibonacci number
    a = b; // update a to the previous b
    b = next; // update b to the new Fibonacci number
  }
  
}


export function* Fib2() {
	// @TODO
  //implement Closed-form expression 
  let a= (1+ Math.sqrt(5)) / 2; // Golden ratio
  let b= (1- Math.sqrt(5)) / 2; // Negative golden
  let n= 1;
  
  while (true) {
    const next = Math.round((Math.pow(a, n) - Math.pow(b, n)) / Math.sqrt(5));
    yield next; // yield the next Fibonacci number
    n++; // increment n for the next Fibonacci number
  }
  

}
