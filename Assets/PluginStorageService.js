function base64ToBase64Url(base64) {
    return base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function base64UrlToBase64(base64Url) {
    return base64Url.replace(/-/g, '+').replace(/_/g, '/') + '=='.slice((base64Url.length + 3) % 4);
}
class PluginStorageService {
  static lastJobId = 0;
  static jobCallbacks = [];

  // Call a method of this service in Delphi. The last param must always be a callback.
  static callMethod(methodName, ...params) {
    const jobId = ++this.lastJobId;
    this.jobCallbacks[jobId] = params.pop();
    // Note: method params are passed as a base64 encoded JSON array 
    const url = `file://storage-service/${jobId}/${methodName}/${base64ToBase64Url(btoa(JSON.stringify(params)))}`;
    console.log('PluginStorageService:'+url);
    
    window.location.href = url;
  }

  // This is called from Delphi to return result of a method call.
  static returnResult(jobId, result) {
    console.log('PluginStorageService.returnResult:jobId='+jobId+':result='+result);
    if(this.jobCallbacks[jobId]) {
      this.jobCallbacks[jobId](result);
    }
  }

  // Wrapper methods to hide the method call protocol
  static writeLayoutValue(key, value, callback = () => { }) {
    this.callMethod('WriteLayoutValue', key, value, callback)
  }

  static readLayoutValue(key, callback = (value) => { }) {
    this.callMethod('ReadLayoutValue', key, callback)
  }
}
