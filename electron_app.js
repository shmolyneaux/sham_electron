const { app, BrowserWindow, protocol, session } = require('electron');
const fs = require('fs');
const path = require('path');

function onFileSystemChange(filename, fn) {
    fs.watch(
        filename,
        {
            encoding: 'utf8',
            recursive: true,
        },
        (eventType, filename) => {
            console.log(eventType, filename);
            if (filename) {
                fn(filename);
            }
        }
    );
}

liveReloadUpdateCount = 0;

function createWindow () {
  // Create the browser window.
  const win = new BrowserWindow({
    width: 800,
    height: 600,
    frame: false,
    backgroundColor: '#002b36',
    webPreferences: {
      nodeIntegration: true
    }
  })

  // and load the app.html of the app.
  win.loadFile('app.html');

  onFileSystemChange('.', (filename) => {
      if (filename.endsWith(".js") || filename.endsWith(".html")) {
          let timeoutCount = liveReloadUpdateCount + 1;
          liveReloadUpdateCount = timeoutCount;
          setTimeout(() => {
              if (timeoutCount == liveReloadUpdateCount) {
                      console.log("Reloading on file change: ", filename);
                      win.reload();
              }
          }, 100);
      }
  });

  // Open the DevTools.
  // win.webContents.openDevTools()
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(createWindow);

// Quit when all windows are closed.
app.on('window-all-closed', () => {
  // On macOS it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit();
  }
})

app.on('activate', () => {
  // On macOS it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow();
  }
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.
