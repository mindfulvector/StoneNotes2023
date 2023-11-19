import { getStdin } from 'https://deno.land/x/get_stdin/mod.ts';

const request = await getStdin();

console.log("Hello world from a fake CGI/Typescript!");
console.log(request);

