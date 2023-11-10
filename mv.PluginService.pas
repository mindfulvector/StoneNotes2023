unit mv.PluginService;

interface

type

  // Base class for plugin services. Plugins need to be aware of the potential
  // more than a single data context per layout file.
  //
  // This base class currently does not do anything but could be used when an
  // additional service is implemented as a place to refactor StorageService
  // code to.
  TPluginService = class(TObject)
  end;

implementation


{ TPluginService }


end.
