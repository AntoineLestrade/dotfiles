#+s::
    Gui, Add, ComboBox, vProject Limit, ArchiOnWeb|BDC|Event_Manager
    Gui, Add, Button, default, OK
    GUi, Add, Button, , Cancel
    Gui, Show, , Choose project
    return

ButtonOK:
    Gui, Submit
    If IsLabel("Case-" . Project) {
        Loop 1 {
            Goto Case-%Project%
            Case-ArchiOnWeb:
                RunWait "C:\code\CFGOA\archi-on-web\archi-on-web.Scheduler\bin\Debug\archi-on-web.Scheduler.exe" --job Emailer
                break
			Case-BDC:
				RunWait "C:\Code\Mercedes\bdc\Scheduler\bin\Debug\scheduler.exe" --job Emailer
				break
            Case-Event_Manager:
                RunWait "C:\Code\Mercedes\event-manager\Scheduler\bin\Debug\scheduler.exe" --job Emailer
				break
        }
    }
    else {
        MsgBox, 8208, Project not found, The project %Project% has not been configured yet
    }
    Gui, Destroy
GuiEscape:
GuiClose:
ButtonCancel:
    Gui, Destroy
    return
