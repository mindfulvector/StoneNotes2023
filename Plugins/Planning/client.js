// Connecting to the Delphi server using WebSocket
const ws = new WebSocket('ws://127.0.0.1:64768');

ws.onopen = () => {
    console.log('Connected to the server');
};

ws.onerror = (error) => {
    console.error('WebSocket Error:', error);
};

ws.onmessage = (e) => {
    console.log('Server:', e.data);
};

function sendMessage() {
    const message = {
        message: "Hello from JavaScript client!"
    };

    ws.send(JSON.stringify(message));
}
