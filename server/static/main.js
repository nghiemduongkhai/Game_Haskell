const canvas = document.getElementById("gameCanvas");
const ctx = canvas.getContext("2d");
const status = document.getElementById("status");

const ws = new WebSocket(`ws://${window.location.hostname}:8081`);

ws.onopen = () => {
  console.log("Connected to Haskell server ✅");
  status.textContent = "Connected ✅";
};

ws.onclose = () => {
  status.textContent = "Disconnected ❌";
};

ws.onerror = (e) => {
  console.error("WebSocket error:", e);
  status.textContent = "Connection error";
};

ws.onmessage = (event) => {
  console.log("Message from server:", event.data);
  if (event.data === "reload") {
    console.log("Reload triggered by server!");
    window.location.reload(); // reload tự động khi thay đổi /static
  }
};

let t = 0;

// Thông số thanh (paddle)
const paddleWidth = 10;
const paddleHeight = 100;
const margin = 10; // khoảng cách từ mép canvas
let leftPaddle = { x: margin, y: (canvas.height - paddleHeight) / 2 }; // thanh trái
let rightPaddle = { x: canvas.width - margin - paddleWidth, y: (canvas.height - paddleHeight) / 2 }; // thanh phải
const paddleSpeed = 4;

// Bắt phím
let keys = {};
document.addEventListener("keydown", e => keys[e.key] = true);
document.addEventListener("keyup", e => keys[e.key] = false);

// --- Bắt chuột ---
// Khi di chuột trong canvas → di chuyển thanh tương ứng
let controllingLeft = false; // chuột bên trái
let controllingRight = false; // chuột bên phải

canvas.addEventListener("mousemove", e => {
  const rect = canvas.getBoundingClientRect();
  const y = e.clientY - rect.top;
  const x = e.clientX - rect.left;

  // Xác định bên nào điều khiển
  if (x < canvas.width / 2) {
    controllingLeft = true;
    controllingRight = false;
  } else {
    controllingLeft = false;
    controllingRight = true;
  }

  if (controllingLeft) {
    leftPaddle.y = Math.max(0, Math.min(y - paddleHeight / 2, canvas.height - paddleHeight));
  } else if (controllingRight) {
    rightPaddle.y = Math.max(0, Math.min(y - paddleHeight / 2, canvas.height - paddleHeight));
  }
});

// --- Bắt cảm ứng ---
canvas.addEventListener("touchmove", e => {
  const rect = canvas.getBoundingClientRect();
  const touch = e.touches[0];
  const y = touch.clientY - rect.top;
  const x = touch.clientX - rect.left;

  if (x < canvas.width / 2) {
    leftPaddle.y = Math.max(0, Math.min(y - paddleHeight / 2, canvas.height - paddleHeight));
  } else {
    rightPaddle.y = Math.max(0, Math.min(y - paddleHeight / 2, canvas.height - paddleHeight));
  }

  e.preventDefault(); // tránh cuộn trang
});

function draw() {
  // xóa canvas
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  // --- vẽ nét đứt ở giữa ---
  ctx.strokeStyle = "#ffffff";
  ctx.lineWidth = 2;
  ctx.setLineDash([10, 10]);
  ctx.beginPath();
  ctx.moveTo(canvas.width / 2, 0);
  ctx.lineTo(canvas.width / 2, canvas.height);
  ctx.stroke();

  // --- vẽ quả bóng ---
  ctx.setLineDash([]); // reset nét đứt cho các đối tượng khác
  ctx.fillStyle = "#bb0000";
  ctx.strokeStyle = "#ffffff";
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.arc(320 + Math.sin(t) * 100, 240, 10, 0, Math.PI * 2);
  ctx.fill();
  ctx.stroke();

  // --- vẽ thanh trái ---
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(leftPaddle.x, leftPaddle.y, paddleWidth, paddleHeight);

  // --- vẽ thanh phải ---
  ctx.fillRect(rightPaddle.x, rightPaddle.y, paddleWidth, paddleHeight);

  // --- di chuyển thanh dựa trên phím ---
  if (keys["w"] && leftPaddle.y > 0) leftPaddle.y -= paddleSpeed;
  if (keys["s"] && leftPaddle.y + paddleHeight < canvas.height) leftPaddle.y += paddleSpeed;

  if (keys["ArrowUp"] && rightPaddle.y > 0) rightPaddle.y -= paddleSpeed;
  if (keys["ArrowDown"] && rightPaddle.y + paddleHeight < canvas.height) rightPaddle.y += paddleSpeed;

  t += 0.05;
  requestAnimationFrame(draw);
}

draw();