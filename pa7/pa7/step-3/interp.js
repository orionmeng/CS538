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
    switch (String.fromCharCode(bytes[i])) {
      case "<":
        prog.push(Ops.Left);
        break;
      case ">":
        prog.push(Ops.Right);
        break;
      case "+":
        prog.push(Ops.Add);
        break;
      case "-":
        prog.push(Ops.Sub);
        break;
      case "[":
        prog.push(Ops.LBrack);
        break;
      case "]":
        prog.push(Ops.RBrack);
        break;
      case ".":
        prog.push(Ops.Output);
        break;
      case ",":
        prog.push(Ops.Input);
        break;
      default:
        break;
    }
    i += 1;
  }
  return prog;
};

const align_brackets = (prog) => {
  const bmap = {};
  const stack = [];
  for (let i = 0; i < prog.length; i++) {
    if (prog[i] === Ops.LBrack) {
      stack.push(i);
    } else if (prog[i] === Ops.RBrack) {
      const openIndex = stack.pop();
      bmap[openIndex] = i;
      bmap[i] = openIndex;
    }
  }
  return bmap;
};

const bf_eval = (prog, bmap) => {
  const cells = new Uint8Array(10000);
  let cc = 0;
  let pc = 0;
  while (pc < prog.length) {
    switch (prog[pc]) {
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
          pc = bmap[pc];
        }
        break;
      case "]":
        if (cells[cc] !== 0) {
          pc = bmap[pc];
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
  const prog = create_program(bytes);
  const bmap = align_brackets(prog);
  bf_eval(prog, bmap);
};

exports.run = run;
