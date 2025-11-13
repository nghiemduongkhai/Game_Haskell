// =============================================================================
//  DOM Elements & Global State
// =============================================================================
const canvas = document.getElementById("gameCanvas");
const ctx = canvas.getContext("2d");
const status = document.getElementById("status");

// Views
const lobbyView = document.getElementById("lobby");
const gameView = document.getElementById("game");

// Lobby UI
const welcomeMsg = document.getElementById("welcomeMsg");
const playAIBtn = document.getElementById("playAIBtn");
const createRoomBtn = document.getElementById("createRoomBtn");
const findRoomBtn = document.getElementById("findRoomBtn");
const roomCodeInput = document.getElementById("roomCode");
const lobbyErrorMsg = document.getElementById("lobbyErrorMsg");
const roomIdMsg = document.getElementById("roomIdMsg");
const exitRoomBtn = document.getElementById("exitRoomBtn");

// Game UI
const gameOverMenu = document.getElementById("gameOverMenu");
const rematchBtn = document.getElementById("rematchBtn");
const lobbyBtn = document.getElementById("lobbyBtn");
const gameRoomIdMsg = document.getElementById("gameRoomIdMsg");

// Global State
let gameState = null;
let roomStatus = null;
let player1Name = "Player 1";
let player2Name = "Player 2";
let countdown = 3;
let countdownTimer = null;
let ourPlayerName = localStorage.getItem("playerName");

if (ourPlayerName) {
  welcomeMsg.textContent = `Welcome to Pong Game, ${ourPlayerName}!`;
}

// =============================================================================
//  WebSocket Connection
// =============================================================================
const ws = new WebSocket(`ws://${window.location.hostname}:8081`);

ws.onopen = () => {
  console.log("Connected to Haskell server ✅");
  status.textContent = "Connected to server ✅";
};

ws.onclose = () => {
  status.textContent = "Disconnected ❌";
  showView("lobby");
  lobbyErrorMsg.textContent = "Connection lost. Please refresh.";
  createRoomBtn.disabled = true;
  findRoomBtn.disabled = true;
};

ws.onerror = (e) => {
  console.error("WebSocket error:", e);
  status.textContent = "Connection error";
  lobbyErrorMsg.textContent = "Could not connect to server.";
};

// =============================================================================
//  Main Message Handler (Server -> Client)
// =============================================================================
ws.onmessage = (event) => {
  const msg = JSON.parse(event.data);
  console.log("From server:", msg);

  switch (msg.tag) {
    case "ServerRoomCreated":
      const roomId = msg.s_roomId;
      gameRoomIdMsg.textContent = `Room ID: ${roomId}`; 
      lobbyErrorMsg.textContent = "";
      player1Name = ourPlayerName;
      player2Name = "Player 2";
      showView("game");
      roomStatus = { tag: "Waiting" };
      break;

    case "ServerJoinedRoom":
      player1Name = msg.s_p1_name;
      player2Name = msg.s_p2_name;
      const joinedRoomId = roomCodeInput.value.trim();
      gameRoomIdMsg.textContent = `Room ID: ${joinedRoomId}`;
      showView("game");
      break;
    
    case "ServerPlayerJoined":
      player2Name = msg.s_p2_name;
      break;

    case "ServerError":
      if (gameView.classList.contains("active")) {
          roomStatus = { tag: "Waiting" };
      } else {
          lobbyErrorMsg.textContent = msg.s_errorMsg;
      }
      break;

    case "ServerUpdateState":
      gameState = msg.s_gameState;
      const newStatus = msg.s_roomStatus;
      
      if (newStatus.tag === "Countdown" && (!roomStatus || roomStatus.tag !== "Countdown")) {
        gameOverMenu.style.display = "none";       
        startCountdown();
      }
      
      if (newStatus.tag === "GameOver") {
        gameOverMenu.style.display = "block"; // Hiện các nút bấm

        rematchBtn.disabled = false;         
        rematchBtn.textContent = "Rematch";
      }

      roomStatus = newStatus;
      break;
  }
};

// =============================================================================
//  Client Actions (Client -> Server)
// =============================================================================

// -- Lobby Buttons --
playAIBtn.onclick = () => {
  const aiPlayerName = "AI bot";
  localStorage.setItem("playerName", aiPlayerName);

  // Tạo phòng AI
  ws.send(JSON.stringify({
    action: "createRoom",
    playerName: aiPlayerName,
    aiMode: true 
  }));
};

createRoomBtn.onclick = () => {
  ws.send(JSON.stringify({
    action: "createRoom",
    playerName: ourPlayerName
  }));
};

findRoomBtn.onclick = () => {
  const code = roomCodeInput.value.trim();
  lobbyErrorMsg.textContent = "";
  if (!code) {
    lobbyErrorMsg.textContent = "Please enter a room ID!";
    return;
  }
  
  ws.send(JSON.stringify({
    action: "joinRoom",
    roomId: code,
    playerName: ourPlayerName
  }));
};

// -- Game Over Buttons --
lobbyBtn.onclick = () => {
  ws.send(JSON.stringify({
    action: "LeaveRoom" 
  }));
  
  showView("lobby");
  gameOverMenu.style.display = "none";
  gameState = null;
  roomStatus = null;
  gameRoomIdMsg.textContent = "";
  roomIdMsg.textContent = ""; 
  lobbyErrorMsg.textContent = "";
};

exitRoomBtn.onclick = () => {
  // Chuyển hướng người dùng trở lại trang login
  window.location.href = "login.html";
};

rematchBtn.onclick = () => {
  ws.send(JSON.stringify({
    action: "Rematch"
  }));
  
  rematchBtn.disabled = true;
  rematchBtn.textContent = "Waiting for opponent...";
};


// -- Game Input --
document.addEventListener("keydown", e => {
  if (["w", "s", "ArrowUp", "ArrowDown"].includes(e.key)) {
    if (roomStatus && roomStatus.tag === "Playing") {
      ws.send(JSON.stringify({
        action: "input",
        data: { type: "keydown", key: e.key }
      }));
    }
  }
});

document.addEventListener("keyup", e => {
  if (["w", "s", "ArrowUp", "ArrowDown"].includes(e.key)) {
    if (roomStatus && roomStatus.tag === "Playing") {
      ws.send(JSON.stringify({
        action: "input",
        data: { type: "keyup", key: e.key }
      }));
    }
  }
});

// =============================================================================
//  Helper Functions
// =============================================================================

function showView(view) {
  lobbyView.classList.remove("active");
  gameView.classList.remove("active");
  document.getElementById(view).classList.add("active");
}

function startCountdown() {
  if (countdownTimer) return;
  countdown = 3;
  countdownTimer = setInterval(() => {
    countdown--;
    if (countdown <= 0) {
      clearInterval(countdownTimer);
      countdownTimer = null;
    }
  }, 1000);
}

function drawOverlay(text) {
  ctx.fillStyle = "rgba(0, 0, 0, 0.7)";
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  
  ctx.fillStyle = "white";
  ctx.font = "50px 'Orbitron', sans-serif";
  ctx.textAlign = "center";
  ctx.fillText(text, canvas.width / 2, canvas.height / 2);
}

// =============================================================================
//  Game Render Loop (Vòng lặp Vẽ)
// =============================================================================
function draw() {
  requestAnimationFrame(draw);
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  ctx.strokeStyle = "#ffffff";
  ctx.lineWidth = 2;
  ctx.setLineDash([10, 10]);
  ctx.beginPath();
  ctx.moveTo(canvas.width / 2, 0);
  ctx.lineTo(canvas.width / 2, canvas.height);
  ctx.stroke();
  ctx.setLineDash([]);

  // Lấy tên P1/P2 
  if (!gameState) {
    ctx.font = "20px 'Orbitron', sans-serif";
    ctx.fillStyle = "#ffffff";
    ctx.textAlign = "left";
    ctx.fillText(player1Name, 20, 30);
    ctx.textAlign = "right";
    ctx.fillText(player2Name, canvas.width - 20, 30);
    ctx.textAlign = "center";
    
    if (roomStatus && roomStatus.tag === "Waiting") {
      drawOverlay("Waiting for opponent...");
    }
    return;
  }

  // --- Vẽ các vật thể game ---
  const ball = gameState.ball;
  ctx.fillStyle = "#ff0000";
  ctx.strokeStyle = "#ffffff";
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.arc(ball.ballPos.vecX, ball.ballPos.vecY, ball.ballRadius, 0, Math.PI * 2);
  ctx.fill();
  ctx.stroke();

  const p1 = gameState.paddle1;
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(p1.paddleX, p1.paddleY, p1.paddleWidth, p1.paddleHeight);

  const p2 = gameState.paddle2;
  ctx.fillRect(p2.paddleX, p2.paddleY, p2.paddleWidth, p2.paddleHeight);
  
  // --- Vẽ điểm số ---
  ctx.font = "40px 'Orbitron', sans-serif";
  ctx.fillStyle = "#ffffff";
  ctx.textAlign = "center";
  ctx.fillText(gameState.scoreP1.toString(), canvas.width / 2 - 60, 50);
  ctx.fillText(gameState.scoreP2.toString(), canvas.width / 2 + 40, 50);

  // --- Vẽ Tên người chơi ---
  ctx.font = "20px 'Orbitron', sans-serif";
  ctx.textAlign = "left";
  ctx.fillText(player1Name, 20, 30);
  ctx.textAlign = "right";
  ctx.fillText(player2Name, canvas.width - 20, 30);
  ctx.textAlign = "center";

  // --- Vẽ các Lớp phủ Trạng thái ---
  if (!roomStatus) return;

  switch (roomStatus.tag) {
    case "Waiting":
      drawOverlay("Waiting for opponent...");
      break;
    case "Countdown":
      if (countdown > 0) {
        drawOverlay(`Starting in ${countdown}...`);
      }
      break;
    case "GameOver":
      const winnerName = roomStatus.contents;
      drawOverlay(`${winnerName} wins!`);
      break;
  }
}

draw();