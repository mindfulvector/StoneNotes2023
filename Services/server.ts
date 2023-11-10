// System / third party
import { serve } from "https://deno.land/std/http/server.ts";
import { parse } from "https://deno.land/std/flags/mod.ts";
import { extname, join, relative, resolve } from "https://deno.land/std/path/mod.ts";

// Local modules
import { DateUtils } from "./DateUtils.lib.ts";

// Arguments to this script, which forms the root level backend process for StoneNotes,
// are usually passed in via the server_monitor.c program. See that program for details
// of the arguments passed here.
const args = parse(Deno.args);

let serviceRoot = (args._[0] as string).replaceAll('\\', '/');
let pluginRoot = (args._[1] as string).replaceAll('\\', '/');

if(serviceRoot.indexOf('/') != serviceRoot.Length - 1) {
  serviceRoot += '/';
}
if(pluginRoot.indexOf('/') != pluginRoot.Length - 1) {
  pluginRoot += '/';
}

console.log('serviceRoot: ' + serviceRoot);
console.log('pluginRoot: ' + pluginRoot);

const isStaticResourceExt = (ext: string) => ["html", "js", "css", "png", "jpg", "gif"].includes(ext);

// Function to print a divider bar with centered date and time
function printBarWithDateAndTime() {
    const dateStr = DateUtils.dateToday() + " " + DateUtils.timeNow();
    const barLength = 60;
    const padding = Math.floor((barLength - dateStr.length) / 2);
    const bar = "*".repeat(padding) + dateStr + "*".repeat(barLength - padding - dateStr.length);
    console.error(bar);
}

// Listen to stdin for console commands
// Pressing enter with a blank line will print the divider bar with date and time
async function listenForEnterKeyPress() {
    const buf = new Uint8Array(1024);
    while (true) {
        const n = <number>await Deno.stdin.read(buf);
        const answer = new TextDecoder().decode(buf.subarray(0, n));
        if ("" === answer.trim()) {
            printBarWithDateAndTime();
        } else {
          console.error('Unknown command');
        }
    }
}

listenForEnterKeyPress();

const server = Deno.serve({ port: 64769 }, async (req: Request, info: ServeHandlerInfo) => {
  const arr = req.url.replace('http://','').replace('https://').split("/");
  const uriPath = (arr.length > 1) ? arr.slice(1).join('/').split('?')[0] : '';
  const getParams = (arr.length > 1) ? arr.slice(1).join('/').split('?')[1] : '';
  
  const serviceFsPath = serviceRoot + uriPath;
  const serviceRelPath = relative(serviceRoot, serviceFsPath);

  const pluginFsPath = pluginRoot + uriPath;
  const pluginRelPath = relative(pluginFsPath, pluginFsPath);
  
  if(-1 !== pluginFsPath.indexOf('/favicon.ico')) return new Response("", { status: 404 });

  console.log(`[${DateUtils.dateToday()}] [${DateUtils.timeNow()}] :${req.url}`);
  // console.log(`segments: ${arr.length}`);
  
  // console.log(`serviceRoot: ${serviceRoot}`);
  // console.log(`pluginRoot: ${pluginRoot}`);
  // console.log(`uriPath: ${uriPath}`);
  // console.log(`serviceFsPath: ${serviceFsPath}`);
  // console.log(`pluginRelPath: ${pluginRelPath}`);

  if (uriPath.includes("..")) {
    return new Response("Access denied", { status: 403 });
  }

  try {
    let handled = false;
    // Check service path then plugin path
    for(const fsPath of [serviceFsPath, pluginFsPath])
    {
      // If we found the requested resource, stop looking
      if (handled) break;

      //console.log(`check ${fsPath}`);
      
      // Get info for this specific path interpretation
      try {
        let fileInfo = await Deno.stat(fsPath);
        // Directory listing or index file
        if (fileInfo.isDirectory) {
          const indexFiles = ["index.html", "index.ts"];
          let indexFile;
          for (const file of indexFiles) {
            try {
              await Deno.stat(join(fsPath, file));
              indexFile = file;
              handled = true;
              break;
            } catch (error) {
              // File doesn't exist, continue to the next file
            }
          } // for (const file of indexFiles)

          if (indexFile) {
            handled = true;
            // Handle serving the index file if found
            fileInfo = await Deno.stat(fsPath + '/' + indexFile);
          } else { // if (indexFile) {
            handled = true;
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
          } // if (indexFile) ... else ...
        } // if (fileInfo.isDirectory)

        // Physical file -- possibly an index found in a requested directory above, or a direct
        // file request
        if (fileInfo.isFile) {
          if(fsPath.indexOf('server.ts') !== -1) {
            return new Response("Cannot run the server from itself", { status: 500 });
          }

          const ext = extname(fsPath).slice(1);

          // If the file is a static resource, serve it directly
          if (isStaticResourceExt(ext)) {
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
          } // if (isStaticResourceExt(ext)) {

          // If the file is a .ts file, run it and return the output
          else if (ext === "ts" && -1 === fsPath.indexOf('.lib.ts')) {  // Don't allow executing .lib.ts files
            handled = true;

            const cmd = ["stonenotes_deno", "run", "--allow-read", "--allow-write", "--allow-net", "--allow-run", fsPath]
            const p = Deno.run({
                cmd: cmd,
                stdout: "piped",
                stderr: "piped",
                stdin: "piped",
              });

            const enc = new TextEncoder();
            const decoder = new TextDecoder();

            await p.stdin.write(enc.encode(JSON.stringify({
              method: req.method,
              url: req.url,
              uri: uriPath,
              getParams: getParams,
              serviceRoot: serviceRoot,
              serviceFsPath: serviceFsPath,
              serviceRelPath: serviceRelPath,
              pluginRoot: pluginRoot,
              pluginFsPath: pluginFsPath,
              pluginRelPath: pluginRelPath,
            })));

            p.stdin.close();

            const [stdout, stderr] = await Promise.all([p.output(), p.stderrOutput()]);
            p.close();

            if (stderr.length > 0) {
              await Deno.writeTextFile("error.log", decoder.decode(stderr));
              return new Response("Internal Server Error", { status: 500 });
            } else {
              return new Response(decoder.decode(stdout), { status: 200 });
            }
          } // if (ext === "ts" && -1 === fsPath.indexOf('.lib.ts'))
        } // if (fileInfo.isFile)
      } catch (error) {
        //console.error('!!!!! File access error: '+error);
        continue;
      } // try
    } // for (const fsPath of [serviceFsPath, pluginFsPath])
  } // try
   catch (error) {
    //console.error('!!!!! Unexpected error: '+error);
    return new Response("Not found", { status: 404});
  }

  console.error(`!!!!! File not found: ${uriPath}`);
  return new Response("This kind of file cannot be served", { status: 404});
}); // ... Deno.serve(... (req: Request, info: ServeHandlerInfo) =>
