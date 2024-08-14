"use strict";

const bf_eval = (bytes) => {
  const cells = new Uint8Array(10000);
  let cc = 0;
  let pc = 0;
  while (pc < bytes.length) {
    switch (String.fromCharCode(bytes[pc])) {
      case "<":
        cc = (cc - 1 + cells.length) % cells.length;
        break;
      case ">":
        cc = (cc + 1) % cells.length;
        break;
      case "+":
        cells[cc] = (cells[cc] + 1) % 256;
        break;
      case "-":
        cells[cc] = (cells[cc] - 1 + 256) % 256;
        break;
      case "[":
        if (cells[cc] === 0) {
          let loop = 1;
          while (loop > 0) {
            pc++;
            if (String.fromCharCode(bytes[pc]) === "[") loop++;
            else if (String.fromCharCode(bytes[pc]) === "]") loop--;
          }
        }
        break;
      case "]":
        if (cells[cc] !== 0) {
          let loop = 1;
          while (loop > 0) {
            pc--;
            if (String.fromCharCode(bytes[pc]) === "]") loop++;
            else if (String.fromCharCode(bytes[pc]) === "[") loop--;
          }
        }
        break;
      case ".":
        process.stdout.write(String.fromCharCode(cells[cc]));
        break;
      case ",":
        cells[cc] = fs.readSync(
          process.stdin.fd,
          Buffer.alloc(1),
          0,
          1,
          null
        )[0];
        break;
      default:
        break;
    }
    pc += 1;
  }
};

const run = (bytes) => {
  bf_eval(bytes);
};

exports.run = run;
