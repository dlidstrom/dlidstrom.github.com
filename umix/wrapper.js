// Emulator state
let mem = []; // Memory (array of uint32 arrays)
let reg = new Uint32Array(8); // Registers (8 uint32 values)
let inputBuffer = "";
let inputIndex = 0;
let paused = false; // Tracks if execution is paused for input
let programCounter = 0; // Tracks current instruction pointer
let isPasting = false;
let pasteBuffer = '';
let effectsEnabled = localStorage.getItem('effectsEnabled') !== 'false';;
let onPausedCallback = null;

document.getElementById('effects-toggle').textContent =
  `Effects: ${effectsEnabled ? 'ON' : 'OFF'}`;
document.getElementById('scanline-canvas').style.display =
  effectsEnabled ? 'block' : 'none';
if (!effectsEnabled) {
  document.getElementById('terminal-container').style.filter =
    'drop-shadow(0 0 2px #00ff00)';
}

// Initialize xterm.js
const terminal = new Terminal({
  cursorBlink: true,
  fontFamily: 'monospace',
  theme: {
    background: '#000000',
    foreground: '#00ff00',
  },
  scrollback: 1000,
  allowProposedApi: true
});

const fitAddon = new FitAddon.FitAddon();
terminal.loadAddon(fitAddon);
terminal.open(document.getElementById('terminal-container'));
fitAddon.fit();
terminal.write('\x1b[?2004h');

terminal.onData((data) => {
  if (data.startsWith('\x1b[200~')) {
    const text = data.slice(6).replace('\x1b[201~', '');
    handlePaste(text);
    return;
  }
  switch (data) {
    case '\x7f': // Backspace
      if (inputBuffer.length > 0) {
        inputBuffer = inputBuffer.slice(0, -1);
        terminal.write('\b \b');
      }
      break;
    case '\x1b[A': // Up arrow
    case '\x1b[B': // Down arrow
    case '\x1b[C': // Right arrow
    case '\x1b[D': // Left arrow
      break;

    case '\r':
    case '\n': {
      const line = inputBuffer;
      inputBuffer = '';
      if (paused) {
        terminal.write('\r\n');
        inputBuffer = line + '\n';
        inputIndex = 0;
        resume();
        inputBuffer = '';
      } else if (mem.length > 0 && line.length > 0) {
        inputBuffer = line + '\n';
        runCycle();
        inputBuffer = '';
      }
      break;
    }

    case '\x1b[200~': // Bracketed paste start
      isPasting = true;
      pasteBuffer = '';
      break;

    case '\x1b[201~': // Bracketed paste end
      isPasting = false;
      handlePaste(pasteBuffer);
      pasteBuffer = '';
      break;

    default:
      if (isPasting) {
        pasteBuffer += data;
      } else if (data >= ' ' || data === '\t') {
        inputBuffer += data;
        terminal.write(data);
      }
      break;
  }
});

function handlePaste(text) {
  const lines = text.replace(/\r\n/g, '\n').replace(/\r/g, '\n').split('\n');
  let i = 0;

  function feedNext() {
    if (i >= lines.length) return;
    const printable = lines[i].replace(/[^\x20-\x7e]/g, '');
    i++;
    terminal.write(printable);
    if (i < lines.length) {
      terminal.write('\r\n');
      inputBuffer = printable + '\n';
      inputIndex = 0;
      onPausedCallback = feedNext;
      if (paused) {
        resume();
      } else if (mem.length > 0) {
        runCycle();
      }
    }
  }

  feedNext();
}

// Execute a single instruction and return whether to continue
function step() {
  if (paused || mem.length === 0) return false;

  const op = mem[0][programCounter];
  const a = (op >>> 6) & 0b111;
  const b = (op >>> 3) & 0b111;
  const c = op & 0b111;
  const opcode = op >>> 28;

  switch (opcode) {
    case 0: // Conditional
      if (reg[c] !== 0) reg[a] = reg[b];
      programCounter++;
      break;
    case 1: // Load
      reg[a] = mem[reg[b]][reg[c]];
      programCounter++;
      break;
    case 2: // Store
      mem[reg[a]][reg[b]] = reg[c];
      programCounter++;
      break;
    case 3: // Add
      reg[a] = reg[b] + reg[c];
      programCounter++;
      break;
    case 4: // Multiply
      reg[a] = reg[b] * reg[c];
      programCounter++;
      break;
    case 5: // Divide
      reg[a] = Math.floor(reg[b] / reg[c]);
      programCounter++;
      break;
    case 6: // NAND
      reg[a] = ~(reg[b] & reg[c]);
      programCounter++;
      break;
    case 7: // Halt
      return false;
    case 8: // Malloc
      mem.push(new Uint32Array(Number(reg[c])));
      reg[b] = mem.length - 1;
      programCounter++;
      break;
    case 9: // Free
      mem[reg[c]] = new Uint32Array(0);
      programCounter++;
      break;
    case 10: // Output
      const char = String.fromCharCode(reg[c]);
      if (char === '\r' || char === '\n') {
        terminal.write('\r\n'); // Preserve alignment with CRLF
        programCounter++;
        return 'newline';
      } else {
        terminal.write(char);
        programCounter++;
      }

      break;
    case 11:
      if (inputIndex < inputBuffer.length) {
        reg[c] = inputBuffer.charCodeAt(inputIndex);
        inputIndex++;
        programCounter++;
      } else {
        paused = true;
        if (onPausedCallback) {
          const cb = onPausedCallback;
          onPausedCallback = null;
          setTimeout(cb, 0); // defer so runBatch fully exits first
        }
        return false;
      }
      break;
    case 12: // Program
      if (reg[b] !== 0) {
        mem[0] = new Uint32Array(mem[reg[b]]);
      }
      programCounter = Number(reg[c]);
      break;
    case 13: // Literal
      reg[(op >>> 25) & 0b111] = op & 0x1FFFFFF; // 25-bit mask
      programCounter++;
      break;
    default:
      terminal.write(`\nInvalid instruction: ${opcode}\n`);
      return false;
  }
  return true;
}

// Run the emulator in steps
function runCycle() {
  let lastYield = 0;
  function runBatch() {
    while (true) {
      const result = step();
      if (!result) return; // halted or paused for input
      if (result === 'newline') {
        const now = Date.now();
        if (now - lastYield >= 16) {
          lastYield = now;
          requestAnimationFrame(runBatch); // yield after newline
          return;
        }
      }
    }
  }

  runBatch();
}

// Resume execution after input
function resume() {
  if (!paused) return;
  paused = false;
  runCycle();
}

// Load umix.um file
function loadUmix() {
  fetch('umix.um')
    .then(response => response.arrayBuffer())
    .then(buffer => {
      const bytes = new Uint8Array(buffer);
      const words = [];
      for (let i = 0; i < bytes.length; i += 4) {
        words.push(
          (bytes[i] << 24) |
          (bytes[i + 1] << 16) |
          (bytes[i + 2] << 8) |
          bytes[i + 3]
        );
      }
      mem.push(new Uint32Array(words));

      // Reset emulator state and start
      programCounter = 0;
      paused = false;
      inputBuffer = '';
      inputIndex = 0;

      // Start the emulator after a small delay
      setTimeout(() => {
        runCycle();
      }, 100);
    })
    .catch(err => {
      terminal.write(`Failed to load umix.um: ${err}\n`);
    });
}

// Auto-load umix.um on page load
window.addEventListener("DOMContentLoaded", () => {
  loadUmix();
  terminal.focus();
  initScanlines();
});
const resizeObserver = new ResizeObserver(() => fitAddon.fit());
resizeObserver.observe(document.getElementById('terminal-container'));
terminal.attachCustomKeyEventHandler((e) => {
  if (e.key === 'Tab') {
    return false;
  }
  if ((e.ctrlKey || e.metaKey) && e.key === 'v' && e.type === 'keydown') {
    return false;
  }
  return true;
});
document.getElementById('terminal-container').addEventListener('keydown', (e) => {
  if (e.key === 'Tab') {
    e.preventDefault();
  }
}, true);

function initScanlines() {
  const canvas = document.getElementById('scanline-canvas');
  const gl = canvas.getContext('webgl');

  function resize() {
    canvas.width = canvas.offsetWidth;
    canvas.height = canvas.offsetHeight;
  }
  resize();
  new ResizeObserver(resize).observe(canvas);

  const vert = `
        attribute vec2 a_pos;
        void main() { gl_Position = vec4(a_pos, 0, 1); }
    `;

  const frag = `
        precision mediump float;
        uniform float u_width;
        uniform float u_height;

        void main() {
            vec2 uv = gl_FragCoord.xy / vec2(u_width, u_height);

            // Scanlines
            float line = mod(gl_FragCoord.y, 4.0);
            float scanAlpha = line < 2.0 ? 0.5 : 0.0;

            // Vignette
            vec2 vig = uv * (1.0 - uv.yx);
            float vignette = pow(vig.x * vig.y * 15.0, 0.3);
            float vigAlpha = (1.0 - clamp(vignette, 0.0, 1.0)) * 0.7;

            gl_FragColor = vec4(0.0, 0.0, 0.0, max(scanAlpha, vigAlpha));
        }`;

  const vs = gl.createShader(gl.VERTEX_SHADER);
  gl.shaderSource(vs, vert);
  gl.compileShader(vs);

  const fs = gl.createShader(gl.FRAGMENT_SHADER);
  gl.shaderSource(fs, frag);
  gl.compileShader(fs);

  const prog = gl.createProgram();
  gl.attachShader(prog, vs);
  gl.attachShader(prog, fs);
  gl.linkProgram(prog);
  gl.useProgram(prog);

  const buf = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, buf);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
    -1, -1, 1, -1, -1, 1,
    -1, 1, 1, -1, 1, 1
  ]), gl.STATIC_DRAW);

  const pos = gl.getAttribLocation(prog, 'a_pos');
  gl.enableVertexAttribArray(pos);
  gl.vertexAttribPointer(pos, 2, gl.FLOAT, false, 0, 0);

  gl.enable(gl.BLEND);
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

  const uWidth = gl.getUniformLocation(prog, 'u_width');
  const uHeight = gl.getUniformLocation(prog, 'u_height');
  gl.clearColor(0.0, 0.0, 0.0, 0.0);

  function render() {
    if (effectsEnabled) {
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.uniform1f(uWidth, canvas.width);
      gl.uniform1f(uHeight, canvas.height);
      gl.clear(gl.COLOR_BUFFER_BIT);
      gl.drawArrays(gl.TRIANGLES, 0, 6);
    }

    requestAnimationFrame(render);
  }

  render();
}
const startTime = Date.now();
function animateFlicker() {
  if (effectsEnabled) {
    const t = (Date.now() - startTime) / (Math.random() * 1500 + 10000);
    const flicker = 1.0
      - 0.05 * Math.sin(t * 15.0 / 1.5)
      - 0.02 * Math.sin(t * 0.7 / 1.5)
      - 0.015 * Math.sin(t * 43.0 / 1.5);
    document.getElementById('terminal-container').style.filter =
      `brightness(${flicker}) drop-shadow(0 0 2px #00ff00)`;
  }

  requestAnimationFrame(animateFlicker);
}

animateFlicker();

document.getElementById('effects-toggle').addEventListener('click', () => {
  effectsEnabled = !effectsEnabled;
  localStorage.setItem('effectsEnabled', effectsEnabled);
  document.getElementById('effects-toggle').textContent =
    `Effects: ${effectsEnabled ? 'ON' : 'OFF'}`;
  document.getElementById('scanline-canvas').style.display =
    effectsEnabled ? 'block' : 'none';
  if (!effectsEnabled) {
    document.getElementById('terminal-container').style.filter =
      'drop-shadow(0 0 2px #00ff00)';
  }
});
