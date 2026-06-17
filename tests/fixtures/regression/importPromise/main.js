import { delay } from './imported.js';

async function main() {
    report("before");
    await delay(100);
    report("after");
}

setTimeout(() => { exit(0) }, 200);

main().then(() => { report("then") });
