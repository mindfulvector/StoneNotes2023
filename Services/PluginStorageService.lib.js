const PluginStorageService = function() {
    // So that services know what plugin is requesting the calls.
    // The location.pathname value identifies the plugin, and window.locaton.search
    // identifies the layout file and panel.
    this.pluginContext = window.location.pathname + window.location.search;

    // Connect to the Delphi server using WebSocket
    // TODO: connect to different instance/port based on what Layout Window we are in?
    // Or commuicate between instances/windows on delphi side with only one WS server
    // exposed to plugins.
    this.connected = false;
    this.queue = [];
    this.ws = new WebSocket('ws://127.0.0.1:64768');

    this.ws.onopen = () => {
        this.connected = true;
        console.log('Connected to the server');
        for(task of this.queue) {
            task();
        }
    };

    this.ws.onerror = (error) => {
        this.connected = false;
        console.error('WebSocket Error:', error);
    };

    this.ws.onmessage = (e) => {
        console.log('Server:', e.data);
    };

    this.sendMessage = function() {
        const message = {
            message: `Hello from JavaScript client in pluginContext ${this.pluginContext}!`
        };
        this.ws.send(JSON.stringify(message));
    }

    this.WriteLayoutValue = function(AKey, AValue, ACallback=(result) => {}) {
        // Create a task lambda to either call immediately or queue if not connected yet.
        task = () => {
            ACallback(this.ws.send(JSON.stringify({
                context: this.pluginContext,
                invoke: ["PluginStorageService.WriteLayoutValue", AKey, AValue]
            })));
        };
        
        // Call the task now if we are connected, otherwise put it on the queue
        if(this.connected) task();
        else this.queue.push(task);
    }

    this.ReadLayoutValue = function(AKey, ACallback) {
        // Create a task lambda to either call immediately or queue if not connected yet.
        task = () => {
            ACallback(this.ws.send(JSON.stringify({
                context: this.pluginContext,
                invoke: ["PluginStorageService.ReadLayoutValue", AKey]
            })));
        };

        // Call the task now if we are connected, otherwise put it on the queue
        if(this.connected) task();
        else this.queue.push(task);
    }

    return this;
};
