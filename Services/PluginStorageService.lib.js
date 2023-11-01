export class PluginStorageService extends BaseService {
    WriteLayoutValue(AKey, AValue) {
        return this.ws.send(JSON.stringify({
            context: this.pluginContext,
            invoke: ["PluginStorageService.WriteLayoutValue", AKey, AValue]
        }));
    }

    ReadLayoutValue(AKey) {
        return this.ws.send(JSON.stringify({
            context: this.pluginContext,
            invoke: ["PluginStorageService.ReadLayoutValue", AKey]
        }));
    }
}
