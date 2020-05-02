#!/usr/bin/env node

const { execSync } = require("child_process");
const fs = require('fs');

updateCount = 0;

console.log("Watching src/** for elm file changes");
fs.watch(
    'src',
    {
        encoding: 'utf8',
        recursive: true,
    },
    (eventType, filename) => {
        if (filename && filename.endsWith(".elm")) {
            let timeoutCount = updateCount + 1;
            updateCount = timeoutCount;

            setTimeout(() => {
                // If a newer watch event comes along, then we won't do anything
                // this is needed since changing a single file with elm-vim can
                // cause many filesystem events.
                //
                // I never use node, this works, but I don't want to spend the
                // time to improve this right now.
                if (timeoutCount == updateCount) {
                    console.log('============================================================');
                    console.log(`${filename} updated, rebuilding elm`);
                    // Running elm make directly was giving me weird results in
                    // cygwin. This was also a problem with elm-live...
                    // This _does_ work though...
                    let result = execSync('python -c "import subprocess; subprocess.run(\'elm make src/Main.elm --output=elm.js\')"', {encoding: 'utf8'});
                    console.log(result);
                }
            }, 100);
        }
    }
);

