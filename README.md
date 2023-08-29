# sway

This project is a Haskell library for interacting with the [sway
window manager](https://swaywm.org). It implements a monadic interface
for communicating with the sway daemon via the sway socket.

For example, we can query sway for information about the current session:
```
ghci> withSwaySocket . runSwayT $ getVersion
SwayVersion {versionMajor = 1, versionMinor = 8, versionPatch = 1,
versionHumanReadable = "1.8.1", versionConfigPath =
"/home/me/.config/sway/config"}

ghci> withSwaySocket . runSwayT $ getWorkspaces
[Workspace {wsNum = 1, wsName = "1", wsVisible = True, wsFocused =
True, wsUrgent = False, wsRect = Rectangle {rectX = 5, rectY = 28,
rectWidth = 1910, rectHeight = 1047}, wsOutput = "eDP-1"},Workspace
{wsNum = 2, wsName = "2", wsVisible = False, wsFocused = False,
wsUrgent = False, wsRect = Rectangle {rectX = 5, rectY = 28, rectWidth
= 1910, rectHeight = 1047}, wsOutput = "eDP-1"}]
```

Or we can send commands to the sway daemon:
```
ghci> withSwaySocket . runSwayT $ runCommand "workspace number 2"
```

We can write computations that interact with the sway session using
the `Sway` monad or it's more general `SwayT` cousin.
```
-- Enumerate a list of all workspace and output names.
workspacesAndOutputs :: Sway [String]
workspacesAndOutputs = do
  outputs    <- getOutputs
  workspaces <- getWorkspaces

  return $ (map outputName outputs) ++ (map wsName workspaces)
```
