import * as wasm from "lost-playground";

// REPL state
const repl = {
  elemHistory: document.getElementById("history-text"),
  elemSourceInput: document.getElementById("source-input"),
  inputQueue: [],
  inputHistory: [],
  inputHistoryIndex: 0,
  // Stash user input while we're toggling through history with up/down arrows
  inputStash: "",
};

// ----------------------------------------------------------------------------
// Initialise
// ----------------------------------------------------------------------------

const world = new wasm.World();
repl.elemSourceInput.addEventListener("change", onInputChange);
repl.elemSourceInput.addEventListener("keyup", onInputKeyup);
repl.elemHistory.querySelector("#loading-message").remove();
// Print the init line
init_repl();
repl.elemSourceInput.disabled = false;
repl.elemSourceInput.placeholder =
  "» Type some Lost code and press Enter (Use Shift+Enter for multi-line input)";

function init_repl() {
  const historyItem = document.createElement("div");
  historyItem.classList.add("history-item");
  repl.elemHistory.appendChild(historyItem);
  repl.elemHistory.scrollTop = repl.elemHistory.scrollHeight;
  // Update the input history with a blank line
  repl.inputHistory.push("")
  updateHistoryEntry(repl.inputHistoryIndex, true, wasm.init_repl())
}

// ----------------------------------------------------------------------------
// Handle inputs
// ----------------------------------------------------------------------------

function onInputChange(event) {
  const inputText = event.target.value.trim();
  repl.inputQueue.push(inputText);
  if (repl.inputQueue.length === 1) {
    processInputQueue();
  }
  event.target.value = "";
}

function onInputKeyup(event) {
  const UP = 38;
  const DOWN = 40;
  const ENTER = 13;

  const {keyCode} = event;

  switch (keyCode) {
    case UP:
      if (repl.inputHistoryIndex === repl.inputHistory.length - 1) {
        repl.inputStash = repl.elemSourceInput.value;
      }
      setInput(repl.inputHistory[repl.inputHistoryIndex]);
      if (repl.inputHistoryIndex > 0) {
        repl.inputHistoryIndex--;
      }
      break;

    case DOWN:
      if (repl.inputHistoryIndex === repl.inputHistory.length - 1) {
        setInput(repl.inputStash);
      } else {
        repl.inputHistoryIndex++;
        setInput(repl.inputHistory[repl.inputHistoryIndex]);
      }
      break;

    case ENTER:
      if (!event.shiftKey) {
        onInputChange({target: repl.elemSourceInput});
      }
      break;

    default:
      break;
  }
}

function setInput(value) {
  const el = repl.elemSourceInput;
  el.value = value;
  el.selectionStart = value.length;
  el.selectionEnd = value.length;
}

// Use a queue just in case we somehow get inputs very fast
// We want the REPL to only process one at a time, since we're using some global state.
// In normal usage we shouldn't see this edge case anyway. Maybe with copy/paste?
async function processInputQueue() {
  while (repl.inputQueue.length) {
    const inputText = repl.inputQueue[0];
    repl.inputHistoryIndex = createHistoryEntry(inputText);
    repl.inputStash = "";

    let outputText = "";
    let ok = true;
    if (inputText) {
      try {
        outputText = world.run(inputText);
      } catch (e) {
        outputText = `${e}`;
        ok = false;
      }
    }

    if (outputText) {
      updateHistoryEntry(repl.inputHistoryIndex, ok, outputText);
    }
    repl.inputQueue.shift();
  }
}

// ----------------------------------------------------------------------------
// Rendering
// ----------------------------------------------------------------------------

function createHistoryEntry(inputText) {
  const historyIndex = repl.inputHistory.length;
  repl.inputHistory.push(inputText);

  const firstLinePrefix = '<span class="input-line-prefix">» </span>';
  const otherLinePrefix = '\n<span class="input-line-prefix">… </span>';
  const inputLines = inputText.split("\n");
  if (inputLines[inputLines.length - 1] === "") {
    inputLines.pop();
  }
  const inputWithPrefixes = firstLinePrefix + inputLines.join(otherLinePrefix);

  const inputElem = document.createElement("pre");
  inputElem.innerHTML = inputWithPrefixes;
  inputElem.classList.add("input");

  const historyItem = document.createElement("div");
  historyItem.appendChild(inputElem);
  historyItem.classList.add("history-item");

  repl.elemHistory.appendChild(historyItem);
  repl.elemHistory.scrollTop = repl.elemHistory.scrollHeight;

  return historyIndex;
}

function updateHistoryEntry(index, ok, outputText) {
  const outputElem = document.createElement("pre");
  outputElem.innerHTML = outputText;
  outputElem.classList.add("output");
  outputElem.classList.add(ok ? "output-ok" : "output-error");

  const historyItem = repl.elemHistory.children[index];
  historyItem.appendChild(outputElem);

  repl.elemHistory.scrollTop = repl.elemHistory.scrollHeight;
}
