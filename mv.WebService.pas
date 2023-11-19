unit mv.WebService;

interface

uses
  System.Classes, System.Rtti, System.SysUtils;

type
  TRouteInfo = record
    HttpMethod: string;
    PathPattern: string;
    ControllerClass: string;
    ActionMethod: string;
  end;

function ParseRoutePattern(const Pattern: string): TRouteInfo;
function WebRoute(const HttpMethod, Path, Query, PostBody, RoutePattern: string): string;

implementation

uses
  mv.WebService.HomeController,
  mv.WebService.PluginController;

var
  Controllers: TStringList;

/// <summary>
/// Parse a route in the format GET|POST /path/to/route ControllerClass->ActionMethod
/// </summary>
function ParseRoutePattern(const Pattern: string): TRouteInfo;
var
  Parts: TArray<string>;
begin
  Parts := Pattern.Split([' ', '->']);
  if Length(Parts) <> 4 then
    raise Exception.Create('Invalid route pattern');

  Result.HttpMethod := Parts[0];
  Result.PathPattern := Parts[1];
  Result.ControllerClass := Parts[2];
  Result.ActionMethod := Parts[3];
end;

/// <summary>
/// Check a requested Path against a given RoutePattern in the format
///   GET|POST /path/to/route ControllerClass->ActionMethod
/// If the /path/to/route and request METHOD match then call the ActionMethod on
/// the singleton ControllerClass instance that is created in the initialization
/// section of mv.WebService on startup. The result from this action method is
/// returned as a string.
/// </summary>
function WebRoute(const HttpMethod, Path, Query, PostBody, RoutePattern: string): string;
var
  RouteInfo: TRouteInfo;
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Instance: TValue;
begin
  RouteInfo := ParseRoutePattern(RoutePattern);

  // Match the pattern's request method (GET or POST)
  if not SameText(RouteInfo.HttpMethod, HttpMethod) then
    Exit; // Pattern does not match

  // Match the path pattern with the given path
  if not Path.StartsWith(RouteInfo.PathPattern) then
    Exit; // Pattern does not match

  Context := TRttiContext.Create;
  try
    // Get the singleton instance of the controller
    Instance := Controllers.Objects[Controllers.IndexOf(RouteInfo.ControllerClass)];

    // Get the type of the controller instance
    RttiType := Context.GetType(Instance.TypeInfo);

    // Find and call the action method
    Method := RttiType.GetMethod(RouteInfo.ActionMethod);
    if not Assigned(Method) then
      raise Exception.Create('WebRoute error: action method `'+RouteInfo.ControllerClass+'.'+RouteInfo.ActionMethod+'` not found');

    // Invoke the action method and return the result as a string
    if ('POST' = HttpMethod) then
      Result := Method.Invoke(Instance, [HttpMethod, Path, Query, PostBody]).AsString
    else
      Result := Method.Invoke(Instance, [HttpMethod, Path, Query]).AsString;

  finally
    Context.Free;
  end;
end;

initialization

Controllers := TStringList.Create;
Controllers.AddObject(THomeController.ClassName, THomeController.Create);
Controllers.AddObject(TPluginController.ClassName, TPluginController.Create);

end.
