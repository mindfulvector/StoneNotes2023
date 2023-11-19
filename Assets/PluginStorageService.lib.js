const PluginStorageService = function() {
    // So that services know what plugin is requesting the calls.
    // The location.pathname value identifies the plugin, and window.locaton.search
    // identifies the layout file and panel.
    //this.pluginContext = btoa(window.location.pathname + window.location.search.replace('?', '')).replaceAll('=', '*').replaceAll('/', '-').replaceAll('+', '.');
    this.pluginContext = window.location.pathname + window.location.search.replace('?', '');

    this.WriteLayoutValue = function(AKey, AValue, ACallback=(result) => {}) {
        // Create a task lambda to either call immediately or queue if not connected yet.
        $.post('/Service/PluginStorageService/WriteLayoutValue',
            {
                AContext:   this.pluginContext,
                AKey:       AKey,
                AValue:     AValue
            },
            ACallback);
    };
    
    this.ReadLayoutValue = function(AKey, ACallback) {
        $.get('/Service/PluginStorageService/ReadLayoutValue',
            {
                AContext:   this.pluginContext,
                AKey:       AKey
            },
            ACallback);
    };

    return this;
};
