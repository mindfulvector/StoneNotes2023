class BaseService {
    constructor() {
        // So that services know what plugin is requesting the calls.
        // The location.pathname value identifies the plugin, and window.locaton.search
        // identifies the layout file and panel.
        this.pluginContext = window.location.pathname + window.location.search;

        // Connect to the Delphi server using WebSocket
        // TODO: connect to different instance/port based on what Layout Window we are in?
        // Or commuicate between instances/windows on delphi side with only one WS server
        // exposed to plugins.
        this.ws = new WebSocket('ws://127.0.0.1:64768');

        this.ws.onopen = () => {
            console.log('Connected to the server');
        };

        this.ws.onerror = (error) => {
            console.error('WebSocket Error:', error);
        };

        this.ws.onmessage = (e) => {
            console.log('Server:', e.data);
        };
    }

    sendMessage() {
        const message = {
            message: `Hello from JavaScript client in pluginContext ${this.pluginContext}!`
        };

        this.ws.send(JSON.stringify(message));
    }
}
