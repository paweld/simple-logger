`LPDLogU` - Simple, thread-safe logger for `FPC`/`Lazarus`. 

Logs saved to file, support for log archiving and compression.   

File log name is application file name with `.log` extension.

**How to use**

- add `LPDLogU` unit to `uses` section

- add log with:

  ```pascal
  Logger.AddLog('Some text', ltError);
  //or with params
  Logger.AddLog('A: %d; B: %s; C: %s', [23, 'test', 'name'], ltDebug);
  ```

  Log types:

  - `ltInfo`
  - `ltError`
  - `ltDebug`
  - `ltWarning`
  - `ltFatal`
  - `ltTrace`
  - `ltSuccess`
  - `ltCritical`
  - `ltException`

  

  or
  
  ```pascal
  Logger.AddInfo('Some text');
  Logger.AddDebug('Some text');
  Logger.AddError('%d %d %d %s %s', [1, 2, 3, 'a', 'b']);
  //etc.
  ```
  
  