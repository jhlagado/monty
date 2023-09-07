var stdin = process.stdin;
stdin.setRawMode(true);
stdin.resume();
stdin.setEncoding('utf8');

ansiClearScreen = () => {
    process.stdout.write('\u001b[2J\u001b[H');
}

ansiGoto = (row, col) => {
    process.stdout.write(`\u001b[${row};${col}H`);
}

ansiCursor = (show) => {
    process.stdout.write(`\u001b[?25${show ? 'h' : 'l'}`);
}

ansiClearLine = () => {
    process.stdout.write(`\u001b[0K`);
}

const numRows = 25;
let pOffset = 0;
let c = 1;
let r = 1;
let p = 0;
let text = 'hello\nthere how\nare you going today.\nhello1\nhello2\nhello3\nhello4\nhello5\nthere!!!';


const adjust = () => {
    if (r < 1) r = 1;
    if (c < 1) {
        if (r > 1) {
            c = 127;
            r--;
        } else {
            c = 1
        }
    }
    let r1 = 1;
    let c1 = 1;
    for (let p1 = 0; p1 < text.length; p1++) {
        const ch = text.charAt(p1);
        if (r === r1) {
            if (ch === '\n' && c > c1) { // found c for end of current line
                c = c1;
            }
            if (c === c1) { // found p for r and c
                p = p1;
                return;
            }
        }
        if (ch === '\n') { // if newline then increase row and set col to start of line
            c1 = 1;
            r1++
        }
        else if (ch >= ' ') { // increase column if char is printable
            c1++
        }
    }
    c = c1;
    r = r1;
    p = text.length;
}

const drawText = () => {
    adjust();
    ansiCursor(false)
    ansiGoto(0, 0);
    for (let p1 = pOffset; p1 < text.length; p1++) {
        const ch = text.charAt(p1);
        if (ch === '\n') { // if newline then increase row and set col to start of line
            ansiClearLine();
            process.stdout.write("\n");
        } else {
            process.stdout.write(ch)
        }
    }
    ansiGoto(r, c);
    ansiCursor(true)
}

ansiClearScreen();
drawText();
stdin.on('data', (key) => {

    if (key === '\u0003') process.exit(); // ctrl-c
    else if (key === '\u001b[A' || key === '\u0010') r--; // up or ^P
    else if (key === '\u001b[B' || key === '\u000E') r++; // down or ^N
    else if (key === '\u001b[C' || key === '\u0006') c++; // right or ^F
    else if (key === '\u001b[D' || key === '\u0002') c--; // left or ^B
    else if (key === '\u000D') { // carriage return
        text = text.slice(0, p) + "\n" + text.slice(p);
        c = 1;
        r++;
    }
    else if (key === '\u007F' || key === '\u0008') { // del or bs
        if (p === 0) return;
        const p1 = p;
        c--;
        if (r > 1) adjust();
        text = text.slice(0, p1 - 1) + text.slice(p1 - 1 + 1);
    }
    else if (key >= ' ') {
        text = text.slice(0, p) + key + text.slice(p);
        c++;
    }
    drawText();
});