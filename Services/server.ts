import { serve } from "https://deno.land/std/http/server.ts";
import { parse } from "https://deno.land/std/flags/mod.ts";
import { extname, join, relative, resolve } from "https://deno.land/std/path/mod.ts";

const args = parse(Deno.args);
const docRoot = args._[0] as string;


const isValidExt = (ext: string) => ["html", "js", "css", "png", "jpg", "gif"].includes(ext);
const isBlockedExt = (ext: string) => ["txt", "ini"].includes(ext);

// For todays date;
Date.prototype.today = function () { 
    return ((this.getDate() < 10)?"0":"") + this.getDate() +"/"+(((this.getMonth()+1) < 10)?"0":"") + (this.getMonth()+1) +"/"+ this.getFullYear();
}

// For the time now
Date.prototype.timeNow = function () {
     return ((this.getHours() < 10)?"0":"") + this.getHours() +":"+ ((this.getMinutes() < 10)?"0":"") + this.getMinutes() +":"+ ((this.getSeconds() < 10)?"0":"") + this.getSeconds();
}

const server = Deno.serve({ port: 64769 }, async (req: Request, info: ServeHandlerInfo) => {
  const arr = req.url.replace('http://','').replace('https://').split("/");
  const uriPath = (arr.length > 1) ? arr[1] : '';
  const fsPath = docRoot + uriPath;
  const relPath = relative(docRoot, fsPath);
  
  console.log(`Request on ${new Date().today()} at ${new Date().timeNow()}: req.url: ${req.url}`);
  // console.log(`segments: ${arr.length}`);
  
  // console.log(`docRoot: ${docRoot}`);
  // console.log(`uriPath: ${uriPath}`);
  // console.log(`fsPath: ${fsPath}`);

  if (relPath.includes("..") || relPath[0] == '/' || relPath[1] == ':' || relPath[0] == '\\') {
    return new Response("Access denied", { status: 403 });
  }

  try {
    let fileInfo = await Deno.stat(fsPath);


    if (fileInfo.isDirectory) {
      const indexFiles = ["index.html", "index.ts"];
      let indexFile;
      for (const file of indexFiles) {
        try {
          await Deno.stat(join(fsPath, file));
          indexFile = file;
          break;
        } catch (error) {
          // File doesn't exist, continue to the next file
        }
      }

      if (indexFile) {
        // Handle serving the index file if found
        fileInfo = await Deno.stat(fsPath + '/' + indexFile);
      } else {
        // No index file found, generate directory listing
        let body = `
        <style>* { font-family: sans-serif; }</style>
        <h1>Directory Listing</h1>
          <table border="1">
            <tr>
              <th>Name</th>
              <th>Type</th>
            </tr>`;
        for await (const dirEntry of Deno.readDir(fsPath)) {
          body += `
            <tr>
              <td><a href="${join(uriPath, dirEntry.name)}">${dirEntry.name}</a></td>
              <td>${dirEntry.isDirectory ? 'Directory' : 'File'}</td>
            </tr>`;
        }
        body += "</table>";
        return new Response(body, { status: 200, headers: { "content-type": "text/html; charset=utf-8" }});
      }
    }

    if (fileInfo.isFile) {
      const ext = extname(fsPath).slice(1);
      if(fsPath.indexOf('server.ts') !== -1) {
        return new Response("Cannot run the server from itself", { status: 500 });
      }

      if (isBlockedExt(ext)) {
        return new Response("Access denied", { status: 403 });
      } else if (isValidExt(ext)) {
        const file = await Deno.readFile(fsPath);
        let contentType = 'text/text';
        switch(ext) {
        case 'html':
          contentType = 'text/html';
          break;          
        case 'jpg':
          contentType = 'jpg';
          break;          
        case 'png':
          contentType = 'image/png';
          break;          
        case 'gif':
          contentType = 'image/gif';
          break;          
        }
        return new Response(file, { status: 200, headers: { "content-type": "${contentType}; charset=utf-8" }});
      } else if (ext === "ts" /* || ext === "php" */) {
        let cmd = [];
        if (ext === "ts") {
          cmd = ["deno", "run", "--allow-read", "--allow-write", "--allow-net", "--allow-run", fsPath]
        } /*else if(ext === 'php') {
          cmd = ["../Support/php/php", fsPath]
        }*/
        const p = Deno.run({
            cmd: cmd,
            stdout: "piped",
            stderr: "piped",
            stdin: "piped",
          });

        const enc = new TextEncoder();
        const decoder = new TextDecoder();

        await p.stdin.write(enc.encode(JSON.stringify({ method: req.method, url: req.url })));
        p.stdin.close();

        const [stdout, stderr] = await Promise.all([p.output(), p.stderrOutput()]);
        p.close();

        if (stderr.length > 0) {
          await Deno.writeTextFile("error.log", decoder.decode(stderr));
          return new Response("Internal Server Error", { status: 500 });
        } else {
          return new Response(decoder.decode(stdout), { status: 200 });
        }
      }
    }
  } catch (error) {
    console.log(error);
    return new Response("Not found", { status: 404});
  }
  return new Response("This kind of file cannot be served", { status: 404});
});
