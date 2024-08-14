"use strict";

const Ops = {
  Left: "Left",
  Right: "Right",
  Add: "Add",
  Sub: "Sub",
  LBrack: "LBrack",
  RBrack: "RBrack",
  Output: "Output",
  Input: "Input",
};

const create_program = (bytes) => {
  const prog = [];
  let i = 0;
  while (i < bytes.length) {
    let currentByte = bytes[i];
    let count = 1;
    switch (currentByte) {
      case '+':
        for (let j = i + 1; j < bytes.length; j++) {
          if (bytes[j] === '+') {
            count++;
          } else {
            break;
          }
        }
        prog.push([Ops.Add, count]);
        i += count;
        break;
      case '-':
        for (let j = i + 1; j < bytes.length; j++) {
          if (bytes[j] === '-') {
            count++;
          } else {
            break;
          }
        }
        prog.push([Ops.Sub, count]);
        i += count;
        break;
      case '<':
        prog.push([Ops.Left]);
        i++;
        break;
      case '>':
        prog.push([Ops.Right]);
        i++;
        break;
      case '[':
        prog.push([Ops.LBrack]);
        i++;
        break;
      case ']':
        prog.push([Ops.RBrack]);
        i++;
        break;
      case '.':
        prog.push([Ops.Output]);
        i++;
        break;
      case ',':
        prog.push([Ops.Input]);
        i++;
        break;
      default:
        i++;
        break;
    }
  }
  return prog;
};

const align_brackets = (prog) => {
  const stack = [];
  for (let i = 0; i < prog.length; i++) {
    if (prog[i][0] === Ops.LBrack) {
      stack.push(i);
    } else if (prog[i][0] === Ops.RBrack) {
      const openIndex = stack.pop();
      const closeIndex = i;
      prog[openIndex][1] = closeIndex;
      prog[closeIndex][1] = openIndex;
    }
  }
};

const bf_eval = (prog) => {
  const cells = new Uint8Array(10000);
  let cc = 0;
  let pc = 0;
  while (pc < prog.length) {
    const [op, operand] = prog[pc];
    switch (op) {
      case Ops.Left:
        cc = (cc - 1 + cells.length) % cells.length;
        break;
      case Ops.Right:
        cc = (cc + 1) % cells.length;
        break;
      case Ops.Add:
        cells[cc] = (cells[cc] + 1) % 256;
        break;
      case Ops.Sub:
        cells[cc] = (cells[cc] - 1 + 256) % 256;
        break;
      case Ops.LBrack:
        if (cells[cc] === 0) {
          pc = operand;
        }
        break;
      case Ops.RBrack:
        if (cells[cc] !== 0) {
          pc = operand;
        }
        break;
      case Ops.Output:
        process.stdout.write(String.fromCharCode(cells[cc]));
        break;
      case Ops.Input:
        cells[cc] = fs.readSync(process.stdin.fd, Buffer.alloc(1), 0, 1, null)[0];
        break;
      default:
        break;
    }
    pc += 1;
  }
};

const run = (bytes) => {
  const prog = create_program(bytes);
  align_brackets(prog);
  bf_eval(prog);
};

exports.run = run;
