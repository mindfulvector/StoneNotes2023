// See general comments in PluginStorageService.lib.js for details about pluginContext
const PluginDialogsService = function() {
    this.pluginContext = window.location.pathname + window.location.search.replace('?', '');
    this.Alert = function(AMessage, ACallback=(result) => {}) {
        $.post('/Services/Dialogs/Alert',
            {
                AContext: this.pluginContext,
                AMessage: AMessage
            },
            ACallback);
    };
    
    return this;
};
