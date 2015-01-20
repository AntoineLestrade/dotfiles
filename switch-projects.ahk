redmine_url_begin = https://code.alten.be/redmine/projects/
redmine_url_end = /issues

;opera_path = "C:\Program Files (x86)\Opera\launcher.exe"
browser_path = "C:\Program Files (x86)\Mozilla Firefox\firefox.exe"
;browser_path = "C:\Program Files (x86)\Opera Developer\launcher.exe"
code_home = C:\code\
conemu_exe = "C:\Program Files\Far Manager\ConEmu64.exe"

#+t::
	MsgBox "Run the example"
	Run, %conemu_exe%   /Dir "C:\Users\Antoine\system-settings" /NoUpdate /cmd {cmd}, Max, conemu_id
	SendInput, git fetch && git prettylog{Enter}
	Run, gvim.exe, "D:\", Max, gvim_id
	WinActivate, ahk_pid %gvim_id%
	WinWaitActive, ahk_pid %gvim_id%
	WinMaximize, ahk_pid %gvim_id%
	SendInput `;vf
	;~ IfWinExist, "cmd"
	;~ {
	;~ }
	;~ else { MsgBox "Error"git fetch && git prettylog
	;~ }
	;~ WinActivate, "cmd"
	;~ WinWaitActive, "cmd"
	;~ SendInput, git fetch && git prettylog{Enter}

return

#+p::
	Gui, Add, DropDownList, vProject, Mercedes_Wips|Oda|Mercedes_BER|Mercedes_LOS|CNOA
	Gui, Add, Button, default, OK
	Gui, Add, Button,,Cancel
	Gui, Show,, Choose project
return

ButtonOK:
	Gui, Submit
	If IsLabel("Case-" . Project) {
		Loop 1 {
			Goto Case-%Project%
		Case-Mercedes_Wips:
			project_name := "mercedes-wips"
			code_path    := code_home . "Mercedes\" . project_name
			redmine_name := project_name . "-ii"
			sol_name     := project_name . ".sln"
			local_url	 := "http://mercedes-wips.local"
			break
		Case-ODA:
			project_name := "archi-on-web"
			code_path	 := code_home . "Ordre des Architectes\" . project_name
			redmine_name := project_name
			sol_name     := project_name . "2.sln"
			local_url	 := "http://www.archi-on-web.local"
			break
		Case-Mercedes_BER:
			project_name := "mercedes-ber"
			code_path	 := code_home . "Mercedes\" . project_name
			redmine_name := project_name
			sol_name     := project_name.sln
			local_url    := "http://mercedes-ber.local"
		Case-Mercedes_LOS:
			project_name := "mercedes-los"
			code_path    := code_home . "Mercedes\" . project_name
			redmine_name := project_name
			sol_name     := project_name . ".sln"
			local_url    := "http://mercedes-los.local"
			break
		Case-CNOA:
			project_name := "cnoa-liste-unique"
			code_path    := code_home . "Ordre des Architectes\" . project_name
			redmine_name := "liste-unique"
			sol_name     := project_name . ".sln"
			local_url    := "http://cnoa-liste-unique.local"
		}
		;MsgBox, %code_path%\%sol_name%
		Gui, Destroy
        Run, %code_path%\%sol_name%
		Run, %browser_path% %redmine_url_begin%%redmine_name%%redmine_url_end%
		Run, %browser_path% %local_url%
		Run, %conemu_exe% /Dir "%code_path%" /NoUpdate /cmd {cmd (Admin)}, Max, conemu_id
		Run, gvim.exe, %code_path%, Max, gvim_id
		WinWait, ahk_pid %gvim_id%
		SendInput `;vf
		WinActivate, ahk_pid conemu_id
		WinWaitActive, ahk_pid %conemu_id%
		SendInput, git fetch && git prettylog{Enter}
	}
	else {
        Gui, Destroy
		MsgBox, 8208, Project not found, The project %Project% has not been configured yet
		; MsgBox Project not found %Project%
    }
	; Clean all
	; ExitApp
GuiEscape:
GuiClose:
ButtonCancel:
	Gui, Destroy

return
