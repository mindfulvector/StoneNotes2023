const PluginStorageService = function() {
    // The pluginContext lets backend services know what plugin is requesting
    // service the calls.
    //
    // The `location.pathname` value identifies the plugin HTML file that is
    // making the request, and `window.locaton.search` identifies the command
    // that requested the plugin: for example the query string might have be
    //      "C=STIC"
    // for the default Sicky Notes corkboard, or possibly
    //      "C=STIC board2"
    // for a second corkboard.
    //
    // The context value is NOT saved anywhere and the format of the query
    // string may change during any revision of StoneNotes, don't rely on
    // the format, treat it as opaque.
    this.pluginContext = window.location.pathname + window.location.search.replace('?', '');

    // Also, it is important to note that the port which backend services run
    // on will always be the same one the plugin and this library file are
    // served from, so a port should *never* be specified in the jQuery
    // post/get URL.
    this.WriteLayoutValue = function(AKey, AValue, ACallback=(result) => {}) {
        // Create a task lambda to either call immediately or queue if not connected yet.
        $.post('/Services/Storage/WriteLayoutValue',
            {
                AContext:   this.pluginContext,
                AKey:       AKey,
                AValue:     AValue
            },
            ACallback);
    };
    
    this.ReadLayoutValue = function(AKey, ACallback) {
        $.get('/Services/Storage/ReadLayoutValue',
            {
                AContext:   this.pluginContext,
                AKey:       AKey
            },
            ACallback);
    };

    return this;
};
