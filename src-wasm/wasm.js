const domConnectionStatus = document.getElementById("connectionStatus");
// const domFirefoxWebSocketBullshitExplainer = document.getElementById("firefoxWebSocketBullshitExplainer");

const domMain = document.getElementsByTagName("main")[0];

let wasm_promise = fetch("/l3p-out-wasm/hello_world.wasm");
let wasm_exports = null;

const text_decoder = new TextDecoder();
const text_encoder = new TextEncoder();

WebAssembly.instantiateStreaming(wasm_promise, {
  core: {
    log: function(ptr, len) {
      const msg = decodeString(ptr, len);
      console.log(msg);
    },
    panic: function (ptr, len) {
      const msg = decodeString(ptr, len);
      throw new Error("panic: " + msg);
    },
    timestamp: function () {
      return BigInt(new Date());
    },
  },
  std: {
    printf: function (ptr, len) {
      const msg = decodeString(ptr, len);
      console.log(msg);
      var stdout = document.getElementById("stdout");
      stdout.value += msg;
      //
    },
  }
}).then(function(obj) {
  wasm_exports = obj.instance.exports;
  window.wasm = obj; // for debugging

  const main = wasm.instance.exports.main;
  main();

});

function getU32Array(ptr, len) {
  if (len === 0) return new Uint32Array();
  return new Uint32Array(wasm_exports.memory.buffer, ptr, len);
}

function decodeString(ptr, len) {
  console.log(ptr, len);
  if (len === 0) return "";
  return text_decoder.decode(new Uint8Array(wasm_exports.memory.buffer, ptr, len));
}

function unwrapString(bigint) {
  const ptr = Number(bigint & 0xffffffffn);
  const len = Number(bigint >> 32n);
  return decodeString(ptr, len);
}

