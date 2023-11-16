unit mv.WebService.HomeController;

interface

type
  THomeController = class
  public
    function Index(const HttpMethod, Path, Query: string): string;
    function AboutMe(const HttpMethod, Path, Query: string): string;
  end;
implementation

{ THomeController }

function THomeController.Index(const HttpMethod, Path, Query: string): string;
begin
  Result := 'Hello world!';
end;

function THomeController.AboutMe(const HttpMethod, Path, Query: string): string;
begin
  Result := '<h1>About Me</h1>I''m mv.';
end;


end.
