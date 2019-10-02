# teamcity-error-reporter

## What is it?

Tool to aid in selenium tests failures investigation. Fetches all necessary data (stacktraces, error messages, artifacts, etc.) and convert it into structured org-mode form for further inspection in emacs.

## Usage

### Quick try (run demo)

 Assume you have Leiningen and emacs installed
 
 1. Clone project and `cd` to its directory
 2. Run emacs and load `report.el` file
 3. Execute `make-teamcity-test-report-demo` command
 You will see a buffer containing report in org-mode
 
### Using in production

 To usage in production (i.e. dump information from real teamcity server), you need some environment setup
 Set env variable `teamcity.server.url` (for example: `http://my.teamcity.server`)
 
 1. load report.el file into emacs
 2. Execute `make-teamcity-test-report`
 3. Pass args, buildTypeId and (optional) specific number or id, e.g. "e5_selenium", or "e5_selenium :number 42"
