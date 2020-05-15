
; multi-segment executable file template.

data segment
    
    ;*****************************************************************************
    ; Menu Strings
    ;*****************************************************************************
    
    Menu0 db "***** Welcome to MENU *****",0dH,0aH,'$'
    Menu1 db "                           ",0dH,0aH,'$'
    Menu2 db "Options with spreadsheet:  ",0dh,0ah,'$'
    Menu3 db "1) Import                  ",0dH,0aH,'$'
    Menu4 db "2) Show                    ",0dH,0aH,'$'
    Menu5 db "3) Edit                    ",0dH,0aH,'$'
    Menu6 db "4) Export                  ",0dH,0aH,'$'
    Menu7 db "                           ",0dh,0ah,'$'
    Menu8 db "Other options:             ",0dh,0ah,'$'
    Menu9 db "5) About                   ",0dH,0aH,'$'
    MenuA db "6) Grid                    ",0dH,0aH,'$'
    MenuB db "7) Exit                    ",0dh,0ah,'$'
    
    ;*****************************************************************************
    ; About Strings
    ;*****************************************************************************
                         
    About0 db "***** Final Assignment *****",0dH,0aH,'$'
    About1 db "                            ",0dH,0aH,'$'
    About2 db "Students:        No.:       ",0dH,0aH,'$'
    About3 db "Francisco Rebola 47197      ",0dH,0aH,'$'
    About4 db "Francisco Rois   50027      ",0dH,0aH,'$'
    About5 db "Bruno Vieira     50046      ",0dh,0ah,'$'
    About6 db "                            ",0dh,0ah,'$'
    About7 db "Menu                        ",0dh,0ah,'$'
    
    ;**************************************************************************
    ; Matrix and Formula variables
    ;************************************************************************** 
    
    Matrix db 16 dup(0h)
    FormulaBefore db 5 dup (0h), '$'
    Op1  dw ?
    Op2  dw ?
    Res  dw ?
    RestoDiv dw ?                                                                   
    
    ;**************************************************************************
    ; Grid variables and strings
    ;**************************************************************************
    
    Grid0 db 'A','B','C','D'
    Grid1 db '1','2','3','4'
    Grid2 db "  FORMULA                     RESULT",0dh,0ah,'$' 
    Grid3 db "MENU",0dh,0ah,'$'
    Grid4 db "Press 1 for GridON, 0 for GridOFF.",0dh,0ah,'$'    
    GridON db 0
    MaxColumn dw 0CEh
    MaxRow    dw 24h
    
    ;**************************************************************************
    ; Random and usefull variables
    ;**************************************************************************
    
    NumRead dw ? 
    JumpNum db 0      ; = 0 if cell is 1, 1 if 2, 2 if 3, 3 if 4
    JumpLetter db 0   ; = 0 if cell is A, 1 if B, 2 if C, 3 if D 
    aux dw ?          
    edit_flag db 0
    x_pos db ?
    y_pos db ?
    edited_cell db 0
    position_matrix dw ?
    BlankStringFormula db "     ",0dh,0ah,'$'
    BlankResult  db ?
    BlankFormula db ?
    BlankStringResult  db "      ",0dh,0ah,'$'
    OpBefore db ?
    Op1Before db ?
    Op2Before db ? 
    BlankStringCell    db "    ",0dh,0ah,'$'
    NumAbove127  db "The number is above 127! Try again.",0dh,0ah,'$' 
    NumBelow128  db "The number is below 128! Try again.",0dh,0ah,'$'
    NotNumericValue db "The value isn't numeric! Try again.",0dh,0ah,'$'
    ClearErrorStrings db "                                   ",0dh,0ah,'$'
    
    ;***************************************************************************
    ; Cell and CellValues "Flags"
    ;***************************************************************************     
                 
    Pos_1    db 0
    Pos_2    db 0
    Aux_1    db 0   
    Add_Flag db 0
    Mul_Flag db 0
    Sub_Flag db 0
    Div_Flag db 0
    contador_char db ? 
    NegativeNumber db ?
   
    ;***************************************************************************
    ; Export/Import/Files/Contents Strings
    ;***************************************************************************  
    
    FilenameString db "Insert the filename: ",'$'  
    Filename2 db 50 dup (0h) 
    BufString db 122 dup (0h)  
    StringFor db "FOR:",'$' 
    Bytes2Write dw ?
    FileNotCreated db 0dh,0ah,"Error: File not created.",0dh,0ah,'$'
    FileCreated    db 0ah,"File created successfully!",,0dh,0ah,'$'
    EndExportString db 0dh, 0ah,"Press 'ENTER' for menu.",0dh,0ah,'$'
    FilenameSuccess db ?
    path0 db "C:\emu8086\MyBuild\Contents.bin",0                
    FileNotRead db 0dh,0ah,"Error: File not read.",0dh,0ah,'$' 
    FileRead    db 0dh,0ah,"File read successfully!",0dh,0ah,'$'
    FileNotOpened db 0dh,0ah, "Error: File not opened.",0dh,0ah,'$'
    contador_cell db ?   
    Name2Dest dw ? 
         
    FlagNotNumeric db ?
    Contador_alg db ?                                             
    counterForm dw ?
    
    not_reading_formula db ? 
    Pos_Matrix db ?
                    
    result_update db ?
                    
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
    
    Main:  
    
        call Contents
        
        call Menu
        
    Exit_OS:
    
        mov ax, 4c00h  ; Exit to operating system.
        int 21h
            
    
    ;*****************************************************************************
    ; Menu Routine - Show the mouse-selectable options 
    ; Input: None
    ; Output: None
    ; Destroy: Nothing 
    ;***************************************************************************** 
    
    Menu proc 
        
        mov al, 03h           ; Need to move to al the desired video mote (text mode).
        call SetVideoMode     ; Sets Video Mode.
        
        call ClrScrn          ; Clears the screen.
 
        mov dx, offset Menu0  ; DX gets the address from Menu 0 in order to print the various strings.
        
        MenuCicle: 
        
            cmp dx, offset MenuB  ; Compares if DX has the address of the last String. 
            ja MenuEnd            ; If it does, it stops printing.  
            
            mov ah, 9  ; This interrupt prints the string, which address is stored in DX.
            int 21h
            
            add dx, 30     ; We need to add 30 because each string has that size.
                           ; After adding that value DX has the address of the next string.
            jmp MenuCicle 
             
        MenuEnd:        
        
            MenuCicle2:
            call OptionSelected ; This routine shows the mouse and gets its position when it's pressed by the user.
                                ; CX = column, DX= row    
                                
            cmp dx, 24       ; This cmps checks if the user selected any option. If not, it keeps getting the mouse position, when it's pressed by the user.
            jb MenuCicle2 
            cmp dx, 32
            ja C1
            call Import  ; The user selected the Import Option.
            jmp C7
                
                C1: 
                    
                    cmp dx, 39
                    ja C2
                    call Show   ; The user selected the Show Option.
                    jmp C7
                
                C2:    
                    cmp dx, 47
                    ja C3
                    call Edit   ; The user selected the Edit Option.
                    jmp C7
                    
                C3:
                    cmp dx, 55
                    ja C4        
                    call Export ; The user selected the Export Option.  
                    jmp C7
                    
                C4:
                    cmp dx, 72
                    jb MenuCicle2
                    cmp dx, 79
                    ja C5 
                    call About  ; The user selected the About Option.
                    jmp C7
                
                C5:
                    cmp dx, 87
                    ja C6
                    call GridOption ; The user selected the Grid Options Option.
                    jmp C7
                        
                C6:
                    cmp dx, 96
                    ja MenuCicle2
                    call Exit       ; ; The user selected the Exit Option.
                
                C7: 
                     ret 
            
    Menu endp 
    
    ;***************************************************************************    
    ; Import Spreadsheet Routine - Loads from a text file the spreadsheet values
    ; Input: None
    ; Output: None
    ; Destroys: AX, BX, CX and DX
    ;***************************************************************************    
    
    Import proc
        
       call ClrScrn        ; Clear Screen
       mov cx, 0 
       mov counterForm, 0
       mov dx, offset FilenameString    ; Asks the filename to import from.
       mov ah, 9
       int 21h  
       
       call ReadStringFromKeyboard      ; Reads the name inserted by the user. 
       cmp FilenameSuccess, 1           
       jne ErrorRead                    ; If the name is not valid
                                        ; prints a error string and import ends.
           
       mov dx, offset Filename2         ; DX: ASCIZ filename. Input for Fopen.
                                        ; File to open.
       
       mov al, 0    ; al=0 (reads the file).
       
       call Fopen
       jc FileNotOpen ; If CarryFlag=0, the FileOpen was successfull.
                      ; Returns in ax the file handler.
       
       mov bx, ax     ; bx gets the file handler
                      ; we'll need it later to close the file.
     
      
       call CleanMatrixFormula ; As the file was opened, we need to reset the matrix and
                               ; the formula.
       
       mov cx, 122    ; n of bytes to read.
       mov dx, offset BufString ; DX: ASCIZ filename. Input for Fread.
       
       call Fread     ; Reads the string in the file and send it to BufString.
       jc ErrorRead   ; If CarryFlag=0, the FileRead was successfull.
       call Fclose    ; We need to close the file after we stop using it to read.
     
       mov bx, 0 

       ImportCicle:   ; Cicle to get all the cells values and the formula in the bufString.
                      ; The format must be: CR:CellValue;...;FOR:CR1opCR2
                      ; C=Column, R=Row and op=operation.
                      ; Otherwise the file is corrupted.
                      
                      
       mov cx, 0      ; We'll get in CX the position of the matrix to put the value in.
       
       cmp BufString[bx], ' '
       je EndExportStringJump
       
       cmp BufString[bx], 'F' ; When BufString[bx]='F' we need to check if the FOR exists.
       je CheckIfForExists
       
       cmp BufString[bx], 'A' ; Checks if Column is A.
       jne BufNextChar
       jmp ReadNumFromBufString
       
       BufNextChar:
            
            cmp BufString[bx], 'a'  ; Checks if Column is a (it's case sensitive).
            jne BufNextCharB
            jmp ReadNumFromBufString
            
       BufNextCharB:
            cmp BufString[bx], 'B'  ; Checks if Column is B.
            jne BufNextCharBB  
            inc cx
            jmp ReadNumFromBufString
            
       BufNextCharBB:
            cmp BufString[bx], 'b'  ; Case sensitive.
            jne BufNextCharC
            jmp ReadNumFromBufString
            
       BufNextCharC:
            cmp BufString[bx], 'C'  ; Checks if Column is C.
            jne BufNextCharCC  
            add cx, 2
            jmp ReadNumFromBufString
            
       BufNextCharCC:
            cmp BufString[bx], 'c'  ; Case sensitive.
            jne BufNextCharD
            add cx, 2
            jmp ReadNumFromBufString
            
       BufNextCharD:
            cmp BufString[bx], 'D'  ; Checks if Column is D.
            jne BufNextCharDD 
            add cx, 3
            jmp ReadNumFromBufString
            
       BufNextCharDD:
            cmp BufString[bx], 'd'  ; Case sensitive.
            jne ErrorRead           ; If the Column is not A,B,C or D the file is corrupted.   
            add cx, 3
       ReadNumFromBufString: 
       
            inc bx
            
            cmp BufString[bx], '1'  ; Checks if Row is 1.
            jne BufNextNum2
            jmp CheckIf2p
            
       BufNextNum2:
            
            cmp BufString[bx], '2'  ; Checks if Row is 2.
            jne BufNextNum3 
            add cx, 4               ; If the Row=2, we need to add 4 to position_matrix.
            jmp CheckIf2p
            
       BufNextNum3: 
            
            cmp BufString[bx], '3'  ; Checks if Row is 3.
            jne BufNextNum4
            add cx, 8               ; If the Row=3, we need to add 8 to position_matrix.
            jmp CheckIf2p
            
       BufNextNum4:
            
            cmp BufString[bx], '4'  ; Checks if Row is 4.
            jne ErrorRead
            add cx, 12              ; If the Row=4, we need to add 12 to position_matrix.
            jmp CheckIf2p
            
       CheckIf2p:
            inc bx
            
            cmp BufString[bx], ':' 
            jne ErrorRead
            inc bx
                  
       ReadCellValue:
            
            cmp BufString[bx], ';' 
            jne KeepReading
            inc bx 
            jmp ImportCicle
            
       KeepReading:
            
            call ReadNumFromImport ; Reads the cell value.
            
            push bx                ; BX= BufStringPosition. We need to save it for later.
            mov bx, cx             ; CX= MatrixPosition
            inc cx
            mov Matrix[bx], al     ; AL has the cell value.
            pop bx 
            jmp ImportCicle        ; Time to read the next cell value or formula.
       
       CheckIfForExists:
            
            inc bx
            cmp BufString[bx], 'O' ; Cmps to check if in the file it's 'FOR:'.
            jne ErrorRead
            inc bx
            
            cmp BufString[bx], 'R'
            jne ErrorRead
            inc bx
            
            cmp BufString[bx], ':'
            jne ErrorRead 
            
         ImportForm: 
         
            inc bx
            push cx    ; CX has the position of the Matrix.
            mov cl, BufString[bx]
            push bx    ; BX has the position of the BufString.
            mov bx, counterForm
            mov formulaBefore[bx], cl 
            pop bx
            pop cx
            inc counterForm
            cmp counterForm, 5  ; Formula has 5 chars (CRopCR).
            jne ImportForm
            
            jmp EndExportStringJump 
       
       
       ErrorRead:   ; If a error occured, prints a string of error
                    ; and waits the user to press ENTER to get back to menu.      
            
            mov dx, offset FileNotRead 
            
            mov ah, 9
            int 21h
              
            jmp EndEXS
            
       
       FileNotOpen:
       
            mov dx, offset FileNotOpened
            
            mov ah, 9
            int 21h 
            jmp EndEXS
            
            EndExportStringJump:
            
            mov dx, offset FileRead
            
            mov ah, 9
            int 21h           ; Prints a string saying that the file was read correctly.
            
            EndEXS: 
            mov dx, offset EndExportString ; Same string for import & export.  
            
            mov ah, 9
            int 21h           ; Prints a string asking ENTER to get back to menu. 
            
            Need2PressEnter:  ; Waits for the user to press Enter to get back to menu.
            
            mov ah, 1
            int 21h
            
            cmp al, 0Dh
            je EndImportMenu
            call ClearChar
            jmp Need2PressEnter
            
       EndImportMenu:
            call Menu
      
       ret
        
    Import endp
    
    ;*********************************************************************************    
    ; Show Spreadsheet Routine - Shows the spreadsheet with the data currently loaded.
    ; Input: Variable - edit_flag = 1, it was called in edit function
    ;                             = 0, it was called in menu function
    ;
    ; Output: None
    ; Destroys: AX and BX
    ;*********************************************************************************
     
    Show proc
        
        call ClrScrn  ; Clears the Screen
        
        mov al, 13h        ; Needs to move to al the desired video mode (graphic mode - 13h).
        call SetVideoMode  ; Sets the Video Mode
        
        call DrawSpreadsheet ; This routine draws the spreadsheet.
        
        cmp edit_flag, 1   ; Checks if the Show was called in Menu or in the Edit Routine. 
        je ShowEnd         ; If was called in Edit, there's no need to wait to user press the Menu Button.
        
        call OptionSelected ; Shows the mouse and gets its position, when it's pressed by the user.
        jmp first_cicle  
        
        CicleGetMouse:
        
            call GetMousePos ; There's only need to call the show mouse routine once. In that order, in this cicle we only get the mouse position.
            
            first_cicle: 
            
            cmp bx, 1
            jne CicleGetMouse                 
            
            call MenuSelected ; This routine checks if the user selects the Menu Button.
        
        ShowEnd:     
            ret
    
    Show endp                                                        
    
    ;********************************************************************************************************************************    
    ; Edit Spreadsheet Routine - Starts by showing the spreadsheet. 
    ;                            The user then can change the cells by poiting and writing the integer number they want on that cell. 
    ; Input: None
    ; Output: None
    ; Destroys: AX, BX, CX and DX  
    ;********************************************************************************************************************************
    
    Edit proc    
         
            mov edit_flag, 1  ; This flag is used in the Show Routine.   
            mov bx, 0
                  
            call Show   ; Starts by showing the spreadsheet.
                
         LoopEdit:  ; We need a loop in order to be able to change multiple cells.  
            call OptionSelected  ; Shows the mouse and gets its position, when it's pressed by the user.
             
            NotSelected:
                cmp bx, 1    ; BX=1 if the left cursor of the mouse is pressed. When this happens we need to check what did the user pressed.            
                je Selected   
                
                OffLimitsCicle:       ; If the user pressed a position with nothing we need to get the mouse position again.
                    call GetMousePos
                    jmp NotSelected
                
            Selected:
                
                ; Theres cmps checks if the user selects the formula.
                
                cmp cx, 20h
                jb NextCmp1  ; Needs to check if the user pressed the menu.
                
                cmp cx, 0C8h 
                ja NextCmp2  ; Needs to check if the user pressed the grid.
                
                cmp dx, 69h
                jb NextCmp2  ; Needs to check if the user pressed the grid. 
                
                cmp dx, 79h
                ja NextCmp1  ; Needs to check if the user pressed the menu.
                
                call ReadFormula ; When the user press the formula square. 
                
                NextCmp1: ; Checks if the menu is selected. 
                
                cmp cx, 14h  
                jb NextCmp2
                
                cmp cx, 64h
                ja NextCmp2
                
                cmp dx, 83h
                jb NextCmp2
                
                cmp dx, 97h
                ja NextCmp2
                mov edit_flag, 0 ; Menu pressed. So, we aren't anymore in Edit Function.
                call Menu
                
                NextCmp2:  ; Checks if the grid is selected. 
                           ; If so we need to verify with cell was selected.
                           
                cmp cx, 50h
                jb OffLimitsCicle
                
                cmp cx, 240h
                ja OffLimitsCicle
                
                cmp dx, 14h
                jb OffLimitsCicle
                
                cmp dx, 50h
                ja OffLimitsCicle       
                mov bx, 0          ; BX will be our Matrix_Position. It will be incremented according to the cell selected.
                jmp MaxColumnStart 
                
                MaxColumnInc:         
                   inc JumpLetter     ; JumpLetter=0 if A_Column, 1 if B_Column, 2 if C_Column and 3 if D_Column.
                   add MaxColumn, 7Eh 
                
                    MaxColumnStart:
                        cmp cx, MaxColumn ; Last column of the A,B,C or D cell. 
                        ja MaxColumnInc      
                        jmp MaxRowStart   ; When we get the Letter_Cell we need to get Num_Cell.
                    
                MaxRowInc:
                    inc JumpNum      ; JumpNum=0 if 1_Row, 1 if 2_Row, 2 if 3_Row and 3 if 4_Row.
                    add MaxRow, 10h
                        
                    MaxRowStart:    
                        cmp dx, MaxRow  ; Last row of the 1, 2, 3 or 4 Cell.
                        ja MaxRowInc   
                
                call CellFlags   ; When we get the cell selected, it's time to start editing the cell by writing its value in the middle of the cell.
                
                ; Now it's time to increment BX (Matrix_position) according to the cell selected.
                
                JumpNumCicle:     ; We need to increment Matrix_position by 4, (JumpNum) times.    
                    cmp JumpNum, 0    ; If JumpNum!=0, bx=bx+4
                    je JumpLetterCicle 
                    add bx, 4
                    dec JumpNum
                    jmp JumpNumCicle
                
                JumpLetterCicle:  ; We need to increment Matrix_position by 1, (JumpLetter) times.
                    
                    cmp JumpLetter, 0 ; If JumpLetter!=0, bx++
                    je CellValue
                    inc bx
                    dec JumpLetter
                    jmp JumpLetterCicle
                
                CellValue:
                    ; Cell Selected. Must read from keyboard the number to insert in cell.
                    mov position_matrix, bx 
                    call ReadNumFromKeyboard ; Num is stored in variable: NumRead                         
                    mov ax, NumRead          
                   
                    mov bx, position_matrix
                    mov Matrix[bx],al
                    mov edited_cell, 1
                    
            call PrintMatrix   ; Need to print the matrix everytime we edit a cell.
            
            call ResultUpdate  ; We need to update the result everytime we edit a cell.
            
            ; It's necessary to reset the variables that we used in this routine.
            mov edited_cell, 0 
              
            mov JumpLetter, 0
            mov JumpNum, 0 
            mov MaxColumn, 0CEh
            mov MaxRow, 24h
            
            jmp LoopEdit   
           
        ret
        
    Edit endp                                                          
                                                                    
    ;*********************************************************************
    ; Export Routine - Exports to a text file the spreadsheet values
    ; Input: None
    ; Output: None
    ; Destroys: AX, BX, CX and DX
    ;*********************************************************************                                                                   
    
    Export proc
    
        call ClrScrn     ; Clears the screen. 
        
        mov si, offset BufString
        mov Bytes2Write, 0 
        
        mov dx, offset FilenameString  ; Asks for the filename to export the file to. 
       
        mov ah, 09
        int 21h
        
        call ClearBuffer               
        call ReadStringFromKeyboard    ; Reads the string inserted by the user.
        cmp FilenameSuccess, 1
        jne NotSuccessfull
        
        mov cx, 0 
        mov dx, offset Filename2
        
        call Fcreate     ; If success cf=0, otherwise cf=1.
        jc NotSuccessfull 
        
        mov bx, ax    ; BX gets the file handler stored in AX.
        push  bx      ; And we need to save it for later.
        mov bx, 0     ; BX will be used has the position_matrix.
        
        WrittingCicle:
            cmp Matrix[bx], 0h    ; Checks if the the value in the Matrix is NULL.
            je Incposition        ; If it's NULL, there's no need to export it to a file. 
            push bx          ; We'll need the value of BX later (position_matrix),
            mov ax, bx       ; We have in AX the position of the matrix.
            mov bl, 4        
            div bl           ; We divide AX for 4. The remainder(AH) stands for A, B, C and D.
                             ; The quocient(AL) stands for 1, 2, 3 and 4.
                             ; Example: When position_matrix=14=AX. AX/4. Remainder=2. Quocient=3.
                             ; If remainder=2 and quocient=3, we're talking about the cell: C3.          
            cmp ah, 0
            je PrintA
            
            cmp ah, 1
            je PrintB
            
            cmp ah, 2        ; Cmps to check which Letter of the cell we're exporting.
            je PrintC
            mov [si], 'D' 
            jmp WrittingNum
            
            PrintA:
                mov [si], 'A'   ; Prints the Letter of the cell we're exporting.
                jmp WrittingNum
            PrintB:
                mov [si], 'B'
                jmp WrittingNum
            PrintC:
                mov [si], 'C'
                jmp WrittingNum
            
            WrittingNum:
                inc si
                inc Bytes2Write
                cmp al, 0         ; Cmps to check which Number of the cell we're exporting.
                je Print1
                
                cmp al, 1
                je Print2
                
                cmp al, 2
                je Print3
                mov [si], '4' 
                jmp WrittingMatrix
                
            Print1:               ; Printing the Letter of the cell we're exporting.
                mov [si], '1'
                jmp WrittingMatrix
            Print2:
                mov [si], '2'
                jmp WrittingMatrix
            Print3:
                mov [si], '3'
                jmp WrittingMatrix
                
            WrittingMatrix:       ; After writting in the file, for ex. B3, we need to write ':'.
                inc si         
                inc Bytes2Write
                mov [si], ':'
                inc si         
                inc Bytes2Write   ; The bytes to write in the file increments everytime we write BufString.
                pop bx
                mov cl, Matrix[bx]  
                call int2ascii      ; We need to convert the number signed to a ascii char, in order to write in the file.  
                inc Bytes2Write
                mov [si], ';'       ; After writing B3:Value we need to put ';' in order to start writting another cell.
                inc si         
                inc Bytes2Write
                     
        Incposition:
            inc bx
            cmp bx, 15
            ja ResetBX
            jmp WrittingCicle
            ResetBX:
            mov bx, 0
        
        StopWrittingCells:
        
            cmp FormulaBefore[0], 0h   ; If the formula doesn't exist, we dont need to write 'FOR:' in the file.
            je DontNeedForFile
              
            mov cl, StringFor[bx]    ; Writes 'FOR:' in the file.
            mov [si], cl
            inc si             
            inc Bytes2Write
            inc bx
            cmp bx, 4
            jne StopWrittingCells
            mov bx,0
            
            WriteFormulaCicle: 
                mov cl, FormulaBefore[bx]  ; Writes the Formula in the file.
                mov [si], cl
                inc si     
                inc Bytes2Write
                inc bx
                cmp bx, 5
                jne WriteFormulaCicle   
            
            DontNeedForFile:
            pop bx     
            mov cx, Bytes2Write       ; CX has the number of bytes to write in the file.
            mov dx, offset BufString  ; DX has the string that contains the cells and formula to export.
            call Fwrite               ; Writes in the file the string.
            jmp Successfull    
            
        NotSuccessfull:               ; If a error occured writting in the file, prints a string of error.
            mov dx, offset FileNotCreated 
            mov ah, 9
            int 21h
            jmp EndExport
            
        Successfull:                  ; If the writting proccess was successfull, prints a string saying that.
            mov dx, offset FileCreated
            mov ah, 9
            int 21h
                    
        EndExport:            
            
            mov dx, offset EndExportString ; Appears the option menu if the user wants to go to the menu.
            
            mov ah, 9
            int 21h
            
            NotEnterPressed:
            call ClearChar    ; Clears the incorrect char inserted by the user for a better interface.
                    
            mov ah, 1
            int 21h
            
            cmp al, 0Dh
            jne NotEnterPressed  ; While the ENTER is not pressed, the program doesn't return to menu.
            
            call Menu
            ret
        
    Export endp 
                                                                            
    ;*********************************************************************
    ; About Routine - Prints the name and no. of the elements of the group
    ; Input: SI = offset of strings to print
    ; Output: Nothing
    ; Destroys: AX and DX 
    ;*********************************************************************
 
    About proc
        
        call ClrScrn      ; Clears the screen
         
        mov dx, offset About0     ; DX gets the address of the string About0
        
        AboutCicle:  ; This cicle will print all the strings related to About.
            
            cmp dx, offset About7   
            ja AboutEnd
            
            mov ah, 9
            int 21h
            
            add dx, 31
            jmp AboutCicle
            
        AboutEnd:       ; Checks if the user selects the option Menu.
            
            call GetMousePos
                
            cmp bx, 1
            jne AboutEnd
            
            cmp dx, 56
            jb AboutEnd
            cmp dx, 63
            ja AboutEnd
            
            call Menu 
                              
    ret     
             
    About endp
    
    ;************************************************************************
    ; Exit Routine - Leaves the program and returns to the operating system
    ;                dumping the program context into the file "contents.bin"
    ; Input: None
    ; Output: None
    ; Destroys: The whole program.
    ;************************************************************************   
    
    Exit proc
         
         call exit_func ; Creats the contents.bin
         mov ax, 4c00h  ; Exit to operating system.
         int 21h
        
    Exit endp
 
    ;*********************************************************************************    
    ; Fopen - Opens a file in a specific mode
    ; Input:  AL=Acess and sharing modes
    ;         AL=0, read
    ;         AL=1, write
    ;         AL=2, read/write
    ;         Ds:DX= ASCIZ filename
    ;               
    ; Output: CF=0 if successfull / 1 if error
    ;         AX=file handle if successfull / code error if error
    ;           
    ; Destroys: CF, AX
    ;*********************************************************************************
    
    Fopen proc
        
        
        mov ah, 3Dh
        int 21h
        
        ret
               
    Fopen endp 
    
    ;*********************************************************************************    
    ; Fclose - Closes a file
    ; Input:  BX= File handle
    ;               
    ; Output: CF=0 if successfull / 1 if error
    ;         AX=file handle if successfull / code error if error
    ;           
    ; Destroys: CF, AX
    ;*********************************************************************************
    
    Fclose proc
                 
        mov ah, 3eh
        int 21h
        
        ret
               
    Fclose endp
    
    ;*********************************************************************************    
    ; Fcreate - Creats a file in a specific mode
    ; Input:  CX= File attributes
    ;           =0, normal - no attributes
    ;           =1, read-only 
    ;           =2, hidden
    ;           =4, system
    ;           =7, hidden, system and read-only.
    ;           =16, archive
    ;
    ;         Ds:DX=ASCIZ filename
    ; 
    ; Output: CF=0 if successfull / 1 if error
    ;         AX=file handle if successfull / code error if error
    ;           
    ; Destroys: CF, AX
    ;*********************************************************************************
  
    Fcreate proc
        
        push cx
        
        mov ah, 3Ch
        int 21h
        
        pop cx
        
        ret
               
    Fcreate endp                                                          
 
    ;**********************************************************************************************    
    ; Fread - Reads a string from a file
    ; Input:  BX= file handle
    ;         CX= number of bytes to read
    ;         Ds:DX= Buffer for data
    ;               
    ; Output: CF=0 if successfull / 1 if error
    ;         AX=number of bytes read, 0 if at EndOfFile / Code error if error
    ;           
    ; Destroys: CF, AX
    ;    
    ; Note: data is read beginning at current file position, and the file position is updated after
    ;a successful read the returned AX? may be smaller than the request in CX? if a partial read
    ;occurred  
    ;        
    ; If ax=0 when proc finished, file has ended
    ;**********************************************************************************************
              
    Fread proc
                
        mov ah, 3fh
        int 21h
        
        ret
               
    Fread endp  
                    
    ;**********************************************************************************************    
    ; Fwrite - Writes a string from a file
    ; Input:  BX= file handle
    ;         CX= number of bytes to write
    ;         Ds:DX= Data to write
    ;               
    ; Output: CF=0 if successfull / 1 if error
    ;         AX=number of bytes written / Code error if error
    ;           
    ; Destroys: CF, AX
    ;    
    ; Note: if CX? is zero, no data is written, and the file is truncated or extended to the current
    ;position data is written beginning at the current file position, and the file position is
    ;updated after a successful write the usual cause for AX? < CX? on return is a full disk.
    ;          
    ;**********************************************************************************************
    
    Fwrite proc
               
        mov ah, 40h
        int 21h
        
        ret
               
    Fwrite endp                   
    
    ;*********************************************************************
    ; Show Mouse Pointer 
    ; Input: None
    ; Output:None
    ; Destroys: Nothing
    ;********************************************************************* 
    
    ShowMouse proc
        
        push ax
    	mov ax,01
    	int 33h
    	pop ax
	ret
	
    ShowMouse endp

    ;***********************************************************************************************
    ; Get Mouse Position and Button pressed Routine 
    ; Input: Nothing   
    ; Output:BX- Button pressed (1 - left, 2 - right and 3 - both)
    ; 	     CX- horizontal position (column)
    ; 	     DX- Vertical position (row)
    ; Destroys: Nothing 
    ;*********************************************************************************************** 

    GetMousePos proc
    
	    push ax
	    mov ax,03h
	    int 33h
	    pop ax
	ret
	
    GetMousePos endp    

    ;*********************************************************************
    ; Option Selected Routine: Shows the mouse and gets the mouse position  
    ; Input: None 
    ;                      
    ; Output: CX= Column
    ;         DX= Row
    ;
    ; Destroys: BX 
    ;*********************************************************************

    OptionSelected proc           
           
            call ShowMouse
        
            Cicle:
            
                call GetMousePos
                
                cmp bx, 1    ; While user doesn't press the left cursor (mouse) it keeps getting the mouse position and waiting for the user to press.
                jne Cicle
            
            ret
  
     OptionSelected endp 

                         
    
    ;*************************************************************************    
    ; ClrScrn routine - Clears the screen
    ; Input: None
    ; Ouput: None
    ; Destroys: AX, BX, CX and DX
    ;************************************************************************* 
     
    ClrScrn proc

        mov dh,0
        mov dl,0 
        
        call SetCursorPos ; In order to start clearing the screen at top left (pos: (0,0))
    
    	push ax
    	push bx
    	push cx
    	push dx
    	     
    	mov ah,06
    	mov al,00
    	mov bh,07   ; attributes to be used on blanked lines
    	mov cx,0    ; CH,CL = row,column of upper left corner of window to scroll
    	mov dh,25   ;= row,column of lower right corner of window
    	mov dl,80
    	int 10h
    	
    	pop dx
    	pop cx
    	pop bx
    	pop ax 
    	
    	ret
    endp                                                             
    
    ;*********************************************************************
    ; Set Cursor Position Routine  
    ; Input: DL=Column
    ;        DH=Row
    ; Output: None
    ; Destroys: AX
    ;*********************************************************************   
    
    SetCursorPos proc
        
       
        mov ah, 2
        int 10h
        
        ret
        
    endp                                                       
    
    ;****************************************************************************
    ; Set Video Mode Routine 
    ; Input: AL - Video Mode
    ;           =00h - text mode. 40x25. 16 colors. 8 pages.
    ;           =03h - text mode. 80x25. 16 colors. 8 pages.
    ;           =13h - graphical mode. 40x25. 256 colors. 320x200 pixels, 1 page.
    ; Output: None
    ; Destroys: Nothing
    ;****************************************************************************    
    
    SetVideoMode proc
    
        push ax
        mov ah, 00 
        int 10h
        pop ax
        ret
        
    endp
    
    ;*********************************************************************
    ; Draw Spreadsheet Routine - Draws the entire spreadsheet. 
    ; Input: None
    ; Output: None
    ; Destroys: AX, CX and DX
    ;*********************************************************************
    
    DrawSpreadsheet proc 
        
        call PrintGridABCD1234  ; Prints the A,B,C,D,1,2,3 and 4 of the grid.
        
        cmp GridON, 1     ; Checks if the user wants the grid.
        jne GridOFF
                 
        mov al, 0Fh
        mov cx, 40
        mov dx, 20 
        
        CicleRow:
            call PutPixel
            inc cx
            
            cmp cx, 288 ; 290
            jne CicleRow
            add dx, 15
            mov cx, 40
            cmp dx, 80 ; 82
            jna CicleRow
                     
        mov cx, 40
        mov dx, 20
        
        CicleColumn:
                     
            call PutPixel
            inc dx
            
            cmp dx, 80 ;82       
            jne CicleColumn
            add cx, 62
            mov dx, 20  
            cmp cx, 288  ; 290    
            jna CicleColumn
        
        GridOFF:
            call PrintMatrix    ; Prints the cells value
            call PrintEnter
            call PrintEnter
            call PrintEnter
            call PrintFormulaResult   ; Prints the formula and result.
           
        ret    
        
          
    endp                                                     
    
    ;*********************************************************************
    ; Put Pixel Routine  
    ; Input: AL - Pixel value
    ;        CX - Column
    ;        DX - Row
    ; 
    ; Ouput: None
    ; Destroys: Nothing
    ;*********************************************************************
    
    PutPixel proc
        
        push ax
	    push bx
	    mov ah,0ch
	    mov bh,00 ; active display page
	    int 10h
	    pop bx
	    pop ax
	    ret
        
    endp                                                   
    
    ;**************************************************************************************
    ; Print Matrix Routine - Prints the matrix (each cell value in it's correspondent cell)
    ; Input: None
    ; Output: Nothing
    ; Destroys: AX, BX, CX and DX
    ;**************************************************************************************
       
    PrintMatrix proc 
       
         mov si, offset Matrix
         mov bx, 0              ; counts the column
         mov cx, 0              ; counts the row
         mov dx, 0
         call SetCursorPos
         
         call PrintEnter     
         call PrintEnter                 
         call PrintEnter
            
         PrintCicle:
         
            cmp bx, 0
            jne NotNeeded
            call PrintTab
            NotNeeded:
                    
                    cmp edited_cell, 1  ; If the user edits a cell there's only need to print that cell again.
                    jne PrintAllMatrix
                    
                    cmp position_matrix, si
                    jne NotThisCell
                    
                    PrintAllMatrix:  
                        cmp [si], 128    ; Checks if the number is negative.
                        jb NormalPrint
                        
                        mov al,[si]      ; If it's negative we need to convert al to ax (cbw) in order to get the negative signal.
                        cbw 
                        call PrintSigned ; Prints a negativen number.  
                        jmp NotThisCell
                        
                        NormalPrint:
                        mov al, [si]
                        call Print_Num_Uns  ; Prints a unsigned number.
                        
                        NotThisCell:   
                            inc bx
                            inc si
                                
                            cmp bx, 4
                            jne Tab
                                
                            inc cx
                            cmp cx, 4
                            je FimPrint
                            jmp Enter
            
            Tab:
                call PrintTab
                jmp PrintCicle
                
            Enter:
                call PrintEnter
                call PrintEnter
                
                mov bx, 0
                jmp PrintCicle
                
            FimPrint:
                ret                                         
    endp 
    
    ;*********************************************************************
    ; Print Enter Routine - Prints a ENTER 
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;*********************************************************************
    
    PrintEnter proc 
        
        push ax
        push dx
        
        mov dl, 0Ah
        mov ah, 2
        int 21h
        
        mov dl, 0Dh
        mov ah, 2
        int 21h 
        
        pop dx
        pop ax
        
    ret
        
    endp                                                        
    
    ;*********************************************************************
    ; Print Tab Routine - Prints a TAB 
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;*********************************************************************
    
    PrintTab proc
        
        push ax
        push dx
        
        mov dl, 09h
        mov ah, 2
        int 21h
        
        pop dx
        pop ax
        
    ret
    
    endp                                                         
    
    ;*********************************************************************
    ; Print Space Routine - Prints a SPACE
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;*********************************************************************
     
    PrintSpace proc
        
        push ax
        push dx
        
        mov dl, 32
        mov ah, 2
        int 21h
        
        pop dx
        pop ax
        
        ret
        
    endp                                                          
    
    ;****************************************************************************
    ; Print Grid ABCD1234 Routine - Prints the letters(columns) and numbers(rows)
    ; Input: None
    ; Output: None
    ; Destroys: AX and BX
    ;****************************************************************************
       
    PrintGridABCD1234 proc
        
          mov si, offset Grid0  ; Grid0 has 'A','B','C' and 'D'.
          mov bx, 0
        
          call PrintEnter
          
          PrintCicle2:
                
                call PrintTab
                mov dl, [si]    ; Prints the Letters of the Grid.
                mov ah, 2
                int 21h
                inc bx
                inc si 
                
                cmp bx, 4                             
                jne PrintCicle2 
          
          mov si, offset Grid1  ; Grid1 has '1','2','3' and '4'.
          mov bx, 0
          
          PrintCicle3:
              call PrintEnter
              call PrintEnter
              call PrintSpace
              call PrintSpace
              
              mov dl, [si]      ; Prints the numbers of the Grid.
              mov ah, 2
              int 21h
              inc bx
              inc si
              
              cmp bx, 4
              jne PrintCicle3
                
          ret
                           
    endp

    ;*********************************************************************
    ; Print Formula and Result Routine  
    ; Input: None
    ; Output: None
    ; Destroys: AX, BX, CX and DX
    ;*********************************************************************
   
    PrintFormulaResult proc  
                  
        mov dx, offset Grid2   ; Grid2 has "FORMULA" and "RESULT".
        mov ah, 9
        int 21h 
        
        mov al, 0Fh
        mov bx, 0
        mov cx, 15
        mov dx, 105
        
        CicleRow2:        ; Draws the horizontal lines of the FORMULA rectangle.
        
            call PutPixel
            inc cx
            
            cmp cx, 70
            jne CicleRow2
            
            add dx, 15
            mov cx, 15 
            inc bx 
            
            cmp bx, 2
            jne CicleRow2
        
        mov cx, 240
        mov dx, 105
        mov bx, 0
            
        CicleRow3:     ; Draws the horizontal lines of the RESULT rectangle.  
            
            call PutPixel
            inc cx
            cmp cx, 305
            jne CicleRow3
            
            add dx, 15
            mov cx, 240
            inc bx
            
            cmp bx, 2
            jne CicleRow3
        
        mov cx, 10
        mov dx, 130
        mov bx, 0
        
        CicleRow4:   ; Draws the horizontal lines of the MENU rectangle.
        
            call PutPixel
            inc cx
            cmp cx, 50
            jne CicleRow4
            
            add dx, 20
            mov cx, 10
            inc bx
            
            cmp bx, 2
            jne CicleRow4             
                     
        mov cx, 15
        mov dx, 105   
        mov bx, 0
        
        CicleColumn2:   ; Draws the vertical lines of the FORMULA rectangle.
                     
            call PutPixel
            inc dx
            
            cmp dx, 120
            jne CicleColumn2
            
            add cx, 55
            mov dx, 105
            inc bx
            
            cmp bx, 2
            jne CicleColumn2
                    
        
        mov cx, 240
        mov dx, 105
        mov bx, 0 
         
        CicleColumn3:   ; Draws the vertical lines of the RESULT rectangle.
                                 
            call PutPixel
            inc dx
            
            cmp dx, 120
            jne CicleColumn3
            
            add cx, 65
            mov dx, 105
            inc bx
            
            cmp bx, 2
            jne CicleColumn3 
        
        mov cx, 10
        mov dx, 130
        mov bx, 0
          
        CicleColumn4:    ; Draws the vertical lines of the MENU rectangle.
        
            call PutPixel
            inc dx
            
            cmp dx, 150
            jne CicleColumn4
            
            add cx, 40
            mov dx, 130
            inc bx
            
            cmp bx,2
            jne CicleColumn4
                        
        mov dh, 17 
        mov dl, 2  
        call SetCursorPos
        
        mov dx, offset Grid3
        mov ah, 9
        int 21h 
        
        ; Prints the formula inserted before.
        
        mov dl, 3
        mov dh, 14  
        call SetCursorPos
        
        push bx
        mov bx, 0
        
        PrintFormulaBeforeCicle:
        
        mov dl, FormulaBefore[bx]
        
        mov ah, 2
        int 21h
        
        inc bx
        
        ForgetOperation:
        
        cmp bx, 5
        jne PrintFormulaBeforeCicle
        
        pop bx
        
        ; Prints the result from the formula before.
        mov dl, 32
        mov dh, 14      
        call SetCursorPos
          
        call ResultUpdate
        
        EndXD:   
    ret
        
    endp
    
    ;*********************************************************************
    ; Grid Option Routine - Shows the option to desativate the Grid 
    ; Input: None
    ; Output: None
    ; Destroys: AX and DX
    ;*********************************************************************
        
    GridOption proc
        
        call ClrScrn
        call ClearBuffer
        
        mov dx, offset Grid4
        
        mov ah, 9
        int 21h
        
        GridCicle:
            mov ah, 1
            int 21h
            
            cmp al, '1'
            je GridOptionON 
            
            cmp al, '0'
            je GridOptionOFF
            jmp GridCicle
        
        GridOptionOFF:
            mov GridON, 0
            jmp EndOpt
        
        GridOptionON:
            mov GridON,1
        
        EndOpt:
            call Menu    
            ret   
    
    ;*********************************************************************
    ; Read Number from Keyboard Routine 
    ; Input: None
    ; Output: None
    ; Destroys: AX, BX and CX
    ;*********************************************************************
    
    ReadNumFromKeyboard proc 
        
        push dx
        
        mov ax, 0 
        mov cl, 0 
        mov bx, 0 
        call ClearBuffer
        call ClearErrorString  
        
        call SetCursorPos
        
        mov aux, 0
        mov bx, 10   
        mov NegativeNumber, 0
        
        ReadNumCicle:
            
            mov ah, 1          ; Reads the char inserted by the user (stored in al).
            int 21h 
            
            mov ah, 0  
            
            cmp al, 0Dh        ; Checks when the user pressed ENTER.
            je ReadNumEnd
            
            cmp al, 2Dh             ; Checks if the user entered '-' (negative number).
            je NegativeNumberCicle
                                  
            cmp al, 30h        ; Checks if it's a not numeric value (<'0').
            jb NotNumericNum
                
            cmp al, 39h        ; Checks if it's a not numeric value (>'9'). 
            ja NotNumericNum 

            sub al, 30h        ; AL was in hexadecimal. To change that we subtract 30h.
                           
            mov cx, ax         ; Stores in CX the algarism inserted.
            mov ax, aux        ; AX gets the AUX value.
            push dx   
            mul bx             ; If there's more than 1 algarism, the AX value it's multiplied by 10.
            pop dx       
            mov aux, ax        ; AUX gets the AX value.   
            add aux, cx        ; AUX=AUX+cx                   
            jmp ReadNumCicle          
        
        Error_ReadNum: 
            call PutBlanksString 
            mov dl, 4
            mov dh, 20
            call SetCursorPos
            
            cmp NegativeNumber, 1  ; If it's a negative number, the user inserted a number below -128.
            je NumBelow128C
            
            jmp NumAbove127C 
            
            NumBelow128C:
                mov dx, offset NumBelow128 
                jmp printErrorString
                
            NumAbove127C:
                mov dx, offset NumAbove127  ; If the num read is above 127, it's printed in the screen a error. 
            
            printErrorString:
            
            mov ah, 9
            int 21h
            
            mov NumRead, 0h   ; If there's a error reading the number, numread=0.
            
            jmp ErrorReading
            
        NotNumericNum:
            call PutBlanksString 
            mov dl, 4
            mov dh, 20
            call SetCursorPos
            
            mov dx, offset NotNumericValue  ; If a not numeric value is inserted in the cell, prints a error saying that.
            mov ah, 9
            int 21h  
            
            mov NumRead, 0h   ; If there's a error reading the number, numread=0.
            
            jmp ErrorReading
                
        NegativeNumberCicle:        ; We need to "activate" the negativenumber var.
            mov NegativeNumber, 1
            jmp ReadNumCicle     
        
        ReadNumEnd:            ; If its a negative number, we need to neg AX.
            
            cmp NegativeNumber, 1
            jne NotNegativeNum
            mov ax, aux  
            
            cmp ax, 128        ; Numbers above 128 (below -128) not allowed.
            ja Error_ReadNum
            
            neg ax
            jmp NegativeNum
            
            NotNegativeNum:    ; If it's not a negative number, we dont need to neg AX.
           
                mov ax, aux 
                
                cmp ax, 127    ; Numbers above 127 not allowed.
                ja Error_ReadNum
                
                NegativeNum: 
                mov NumRead, ax   
                
                ErrorReading:
                pop dx
            ret
        
    ReadNumFromKeyboard endp 
   
    ;************************************************************************
    ; Menu Selected Routine - Checks if the user selects the rectangle Menu
    ; Input: CX=Column selected
    ;        DX=Row Selected
    ;
    ; Output: None
    ; Destroys: Nothing
    ;************************************************************************
    
    MenuSelected proc ; if menu is selected calls menu, otherwise doesnt.
        
        cmp cx, 14h
        jb CicleGetMouse
            
        cmp cx, 64h
        ja CicleGetMouse
            
        cmp dx, 83h
        jb CicleGetMouse
            
        cmp dx, 97h
        ja CicleGetMouse
        
        call Menu
        
             ret    
             
    MenuSelected endp
    
    ;***************************************************************************************
    ; Read Formula Routine - Reads the formula inserted by the user in the formula rectangle
    ; Input: Variable - result_update = 1 (Needs to get from the matrix the operands)
    ;                                 -> OpBefore = *,-,+ or /
    ;                                 -> Op1 and Op2 = Operands so we can update the result
    ;
    ;                                 = 0 (Just read from the keyboard the formula inserted)
    ;                   
    ; 
    ; Output: None
    ; Destroys: AX, BX and DX
    ;***************************************************************************************
    
    ReadFormula proc 
        
        cmp result_update, 1
        je Op1Op2
        
        call ClearBuffer      ; Always clear the buffer for some protection.
        call ClearErrorString
        
        ReadFormulaStart:        
            mov bl, 0
            mov contador_char, 0  
           
            mov dl, 3
            mov dh, 14 
            mov BlankFormula, 1
            call PutBlanksString ; Clears the formula rectangle.
            call SetCursorPos
        
        CicleInt:
        
            mov ah, 1
            int 21h ; returns in al    
            
            cmp contador_char, 0
            je MustBeChar
            
            cmp contador_char, 1
            je MustBeNum 
            
            cmp contador_char, 2
            je MustBeOp     
            
            cmp contador_char, 3
            je MustBeChar
            
            cmp contador_char, 4
            je MustBeNum    
            mov Pos_2, bl
                
        MustBeChar:
            
            cmp al, 'A'
            je IncContador
            
            cmp al, 'a'
            je IncContador
            inc bl
            
            cmp al, 'B'
            je IncContador
            
            cmp al, 'b'
            je IncContador
            inc bl
            
            cmp al, 'C'
            je IncContador
            
            cmp al, 'c'
            je IncContador
            inc bl
            
            cmp al, 'D'
            je IncContador
            
            cmp al, 'd'
            je IncContador
            
            jmp ReadFormulaStart
         
        MustBeNum:
            
            cmp al, '1'
            je IncContador
            add bl, 4
            
            cmp al, '2'
            je IncContador
            add bl, 4
            
            cmp al, '3'
            je IncContador
            add bl, 4   
            
            cmp al, '4'
            je IncContador
            
            jmp ReadFormulaStart
          
        MustBeOp:
        
            mov Pos_1, bl
            mov bl, 0
            cmp al, '+'
            jne O1
            mov Add_Flag, 1
            jmp IncContador
            
            
            O1: 
            
                cmp al, '*'  
                jne O2
                mov Mul_Flag, 1 
                jmp IncContador
            
            O2: 
            
                cmp al, '-'
                jne O3
                mov Sub_Flag, 1
                jmp IncContador
            
            O3:
            
                cmp al, '/'
                jne ReadFormulaStart
                mov Div_Flag, 1 
          
            IncContador:             
                push bx
                mov bx, 0
                mov bl, contador_char
                mov FormulaBefore[bx], al
                pop bx
                inc contador_char      
                cmp contador_char, 5
                je EndFormula    
                
                jmp CicleInt    
                             
            EndFormula:       
            
                mov Pos_2, bl  
                
                push bx
                push ax
                
                Op1Op2:
                
                    mov bx, 0
                    mov ax, 0
                    
                    mov bl, Pos_1
                    
                    cmp matrix[bx], 128
                    jb Not_Signed1
                           
                    mov al, Matrix[bx] 
                    cbw
                    mov Op1, ax
                    jmp NextNum
                
                Not_Signed1:
                    mov al, Matrix[bx]
                    mov Op1, ax
                
                NextNum:    
                    mov ax, 0
                    
                    mov bl, Pos_2 
                    
                    cmp matrix[bx], 128
                    jb Not_Signed2 
                     
                    mov al, Matrix[bx] 
                    cbw
                    mov Op2, ax  
                    jmp EndFormula2
                
                Not_Signed2:    
                    mov al, Matrix[bx]
                    mov Op2, ax
                    
                    cmp result_update, 1
                    je NoNeedToCallResultFunc
                
                EndFormula2:    
                pop ax
                pop bx
                call ResultFunc        
                
                NoNeedToCallResultFunc:
        ret
        
    ReadFormula endp                                              
    
    ;*********************************************************************
    ; Result Routine - Calculates the result according to the input.
    ; Input: Variables:  Mul_Flag (Multiplication)
    ;                    Div_Flag (Divison)
    ;                    Add_Flag (Addition)
    ;                    Sub_Flag (Subtraction)
    ;                    Op1 (Operand1)
    ;                    Op2 (Operand2)
    ; 
    ; Output: None 
    ; Destroys: The flags (Mul, Div, Add, Sub), AX and DX
    ;*********************************************************************
    
    ResultFunc proc
         
         ; First we need to clear the result from the formula before
         cmp Mul_Flag, 1
         je OpMul
         
         cmp Add_Flag, 1
         je OpAdd
         
         cmp Sub_Flag, 1
         je OpSub
         
         cmp Div_Flag, 1
         je OpDiv
         jmp EndPrintResult      
         
         OpMul: ; result = operand1*operand2  
         
             mov ax, Op1
             imul Op2         ; (dx ax) = op1*op2 
             mov OpBefore, '*'                 ; dx is ignored because we're working with tiny numbers.
             jmp PrintResult    
            
         OpAdd: ; result = operand1+operand2  
         
             mov ax, Op1
             add ax, Op2
             mov OpBefore, '+'
             jmp PrintResult
         
         OpDiv: ; result = operand1-operand2
         
             mov dx, 0 
             mov ax, Op1
             idiv Op2
             mov OpBefore, '/'        
             jmp PrintResult
                                             
                                             
         OpSub: ; result = operand1/operand2 
             
             mov ax, Op1
             sub ax, Op2 
             mov OpBefore, '-'
             
             
         PrintResult:
             mov Res, ax
             mov RestoDiv, dx
             
         PrintResult2:
             mov dl, 32
             mov dh, 14
             mov BlankResult, 1 
             call PutBlanksString
             call SetCursorPos
             
             mov ax, Res 
             
             cmp ax, 128
             jb Result_Not_Signed
             
             call PrintSigned
             jmp EndPrintResult
             
             Result_Not_Signed: 
                call Print_Num_Uns
             
             EndPrintResult:
             mov Add_Flag, 0
             mov Mul_Flag, 0  
             mov Sub_Flag, 0
             mov Div_Flag, 0 ; We need to reset the flags.
   
    ret   
    
    ResultFunc endp
    
    ;**************************************************************************
    ; Cell Flags Routine - Checks which cell was selected and cleans that cell 
    ; Input: Variable - JumpNum = 0 (Cell is in Row=1)
    ;                           = 1 (Cell is in Row=2)
    ;                           = 2 (Cell is in Row=3)
    ;                           = 3 (Cell is in Row=4)
    ;
    ;                   JumpLetter = 0 (Cell is in Column=A)
    ;                              = 1 (Cell is in Column=B)
    ;                              = 2 (Cell is in Column=C)
    ;                              = 3 (Cell is in Column=D)
    ;
    ; Output: None
    ; Destroys: DX
    ;**************************************************************************
    
    CellFlags proc
                  
        cmp JumpNum, 0
        je ON1
        
        cmp JumpNum, 1
        je ON2 
        
        cmp JumpNum, 2
        je ON3
        jmp ON4
        
        ON1:  
            mov y_pos, 3
            jmp CellFlagsCicle
            
        ON2: 
            mov y_pos, 5
            jmp CellFlagsCicle
            
        ON3: 
            mov y_pos, 7  
            jmp CellFlagsCicle           
            
        ON4: 
            mov y_pos, 9
            
        CellFlagsCicle:
        
            cmp JumpLetter, 0
            je A_ON
            
            cmp JumpLetter, 1
            je B_ON
            
            cmp JumpLetter, 2
            je C_ON
            jmp D_ON
          
        A_ON: 
            mov x_pos, 8
            jmp EndCellFlags 
        
        B_ON: 
            mov x_pos, 16
            jmp EndCellFlags 
        
        C_ON: 
            mov x_pos, 24
            jmp EndCellFlags 
        
        D_ON: 
            mov x_pos, 32
            jmp EndCellFlags 
        
        EndCellFlags:
            mov dh, y_pos 
            mov dl, x_pos
            call SetCursorPos
            call PutBlanksString
            call SetCursorPos
            ret
                      
        
    CellFlags endp 
    
    ;******************************************************************************
    ; Print Num Uns Routine - Prints a Unsigned number
    ; Input: AX = number to print
    ; Ouput: None
    ; Destroys: Nothing
    ;******************************************************************************
    
    Print_Num_Uns proc  
        
        push ax
        push bx
        push cx
        push dx
        
        mov ah,0
        cmp ax,0h
        je if_0
        
        mov cx,10
        mov bx,0 
        
       div_maker:
                 mov dx,0
                 div cx   ; AX=AX/10. DX = remainder.
                 push dx  ; Saving in stack each digit.
                 inc bl   ; Number of divisions.
                 
                 cmp ax,0
                 jne div_maker
                 
       prt_num:       
                 pop ax         ; Getting it back from the stack.
                 add al,30h     ; Converts from int to hexadecimal.
                 
                 ;--printing each digit--
                 mov ah,2
                 mov dl,al ;our digit is placed in al
                 int 21h
                 ;--printing each digit--
                 
                 dec bl
                 
                 cmp bl,0
                 jne prt_num 
                 jmp end_pnu
                 
       if_0:
                 mov dl,0h
                 mov ah,2
                 int 21h          
    
        end_pnu:             
           pop dx
           pop cx
           pop bx
           pop ax
    ret             
    Print_Num_Uns endp
    
    ;**********************************************************************************
    ; PutBlanksString - Prints a blank string 
    ; Input: DL=Column 
    ;        DH=Row
    ;        Variables: BlankFormula = 1 (cleans the formula rectangle)
    ;                                = 0 (doesn't clean)
    ;                   BlankResult  = 1 (cleans the result  rectangle)
    ;                                = 0 (doesn't clean=
    ;                   If BlankFormula & BlankResult = 0 -> Cleans the Cell Rectangle
    ;        
    ; Output: None
    ; Destroys: AX
    ;**********************************************************************************
        
    PutBlanksString proc
        
        call SetCursorPos
        
        push dx
        
        cmp BlankFormula, 1
        je BSF
            
        cmp BlankResult, 1
        je BSR
        
        mov dx, offset BlankStringCell  ; Put blanks in the cell rectangle.
        mov ah, 9
        int 21h
        jmp EndBS
        
        BSR:  ; Put blanks in the result rectangle.
        
            mov dx, offset BlankStringResult
            mov ah, 9
            int 21h
            mov BlankResult, 0
            jmp EndBS
                
        BSF:  ; Put blanks in the formula rectangle.
        
            mov dx, offset BlankStringFormula
            mov ah, 9
            int 21h 
            mov BlankFormula, 0
        
        EndBS:
        pop dx
   
        ret
        
    PutBlanksString endp
  
    ;*****************************************************************************
    ; Result Update Routine - Updates the result according to the existing formula 
    ; Input: Variable: OpBefore (Operation in the formula)
    ;       
    ; Output: None 
    ; Destroys: Nothing
    ;*****************************************************************************
      
    ResultUpdate proc
        
        call GetForOp
                      
        cmp OpBefore, '*'   ; Checks which operation we need to do.
        je MulBefore
        
        cmp OpBefore, '/' 
        je DivBefore
         
        cmp OpBefore, '-'
        je SubBefore
        
        cmp OpBefore, '+'
        je AddBefore
        jmp RUpdateEnd
        
        MulBefore:           ; "Activates" the "flags", in order to know which operation we'll need to do.
            mov Mul_Flag, 1 
            jmp RUpdateEnd
            
        DivBefore:
            mov Div_Flag, 1
            jmp RUpdateEnd
            
        AddBefore:
            mov Add_Flag, 1
            jmp RUpdateEnd
        
        SubBefore:
            mov Sub_Flag, 1
        
        RUpdateEnd:
            push bx
            push ax
            mov bx, 0
            mov ax, 0
                
            mov bl, Pos_1 
            
            cmp Matrix[bx], 128     ; Checks if the operand is negative.
            jb Not_Signed1Update
            
            mov al, Matrix[bx] 
            cbw
            mov Op1, ax 
            jmp NextNum2
            
            Not_Signed1Update:
                mov al, Matrix[bx]
                mov Op1, ax
            
            NextNum2:    
            mov ax, 0
                
            mov bl, Pos_2 
            
            cmp Matrix[bx], 128
            jb Not_Signed2Update
             
            mov al, Matrix[bx] 
            cbw
            mov Op2, ax
            jmp EndUpdate 
            
            Not_Signed2Update:
                mov al, Matrix[bx]
                mov Op2, ax
            
            EndUpdate: 
            pop ax
            pop bx 
                
            call ResultFunc
        
    ret    
    ResultUpdate endp
    
    ;*********************************************************************
    ; PrintSigned Routine - Prints a signed number  
    ; Input: AX=signed number
    ; Output: None
    ; Destroys: Nothing
    ;*********************************************************************
    
    PrintSigned proc  
        
        push ax
        push bx
        push cx
        push dx
        
        cmp ax, 0
        jge L1
        
        mov bx, ax
        
        mov dl, '-'
        mov ah, 2
        int 21h
        
        mov ax, bx
        neg ax
        call PrintSigned
        jmp L3
        
        L1:
            mov dx,0
            mov cx,10
            div cx
            
            cmp ax, 0
            jne L2
            
            add dl, '0'
            mov ah, 2
            int 21h
            
            jmp L3
        
        L2:
            call PrintSigned
            add dl, '0'
            mov ah, 2
            int 21h
            
        L3:
            pop dx
            pop cx
            pop bx
            pop ax
                       
                      
    ret    
    PrintSigned endp                                               
    
    ;*********************************************************************
    ; Clear Error String Routine 
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;*********************************************************************
      
    ClearErrorString proc
        
        push ax
        push dx
        
        mov dl, 4
        mov dh, 20
        call SetCursorPos 
        
        mov dx, offset ClearErrorStrings
        
        mov ah, 9
        int 21h
        
        pop dx
        pop ax
        
    ret
    ClearErrorString endp                                                   
    
    ;*******************************************************************************
    ; Clear Buffer Routine - Clears the buffer from the keyboard for some protection
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;*******************************************************************************
     
    ClearBuffer proc      
        
        push ax
        
        mov ah, 0Ch ;clear
        mov al, 0   ;buffer
        int 21H 
        
        pop ax
    ret
    
    ClearBuffer endp
    
    ;***************************************************************************
    ; Read String From Keyboard Routine 
    ; Input: None
    ; Output: Variable (FilenameSucess).
    ;         If var=1, the filename the user insert is valid, otherwise var=0.        
    ; Destroys: DI 
    ;***************************************************************************
        
    ReadStringFromKeyboard proc ; still need to protect.
        
        push ax 
        push cx
        
        mov Name2Dest, 0
        mov di, offset Filename2
        call CleanFileName     
        mov di, offset Filename2
        
        ReadStringCicle:
            mov ah, 1
            int 21h      ; char stored in al.
             
            cmp al, 0Dh
            je EndReadSCicle
            
            cmp al, '.'
            je ItsALetter
            
            cmp al, 'A'
            jb ErrorNotTxt
            
            cmp al, 'z'
            ja ErrorNotTxt
            
            cmp al, 'Z'
            ja MaybeUncaps
            jmp ItsALetter
            
            MaybeUncaps:
                cmp al, 'a'
                jb ErrorNotTxt
            
            ItsALetter:
            
            mov [di], al
            inc di
            inc Name2Dest
            jmp ReadStringCicle 
        
        EndReadSCicle:

            dec di
            cmp [di], 't'
            jne ErrorNotTxt
            
            dec di
            cmp [di], 'x'
            jne ErrorNotTxt
            
            dec di
            cmp [di], 't'
            jne ErrorNotTxt
            mov FilenameSuccess, 1
            jmp EndEnd
            
            ErrorNotTxt:
                mov FilenameSuccess, 0 
                call CleanFileName
                
            EndEnd:
                pop cx
                pop ax
                ret
                
    ReadStringFromKeyboard endp                                               
 
    ;*********************************************************************
    ; Int to Ascii Uns Routine - Convert a unsigned num to ascii chars 
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;*********************************************************************
    
    int2ascii_uns proc
        
        push ax
        push bx
        push cx
        push dx
        
        mov ah,0
        cmp ax,0h
        je if_0
        
        mov cx,10;"divider"
        mov bx,0 ;counter
        
       div_maker2:
                 mov dx,0
                 div cx   ; ax= ax/10 , dx=remainder (resto)
                 push dx  ; saving ech digit to the stack
                 inc bl   ; give us the number of divisions we made
                 
                 cmp ax,0
                 jne div_maker2
                 
       prt_num2:       
                 pop ax ;gettin it back from the stack
                 add al,30h ; int --> char
                 
                 ;--moving each digit to the string--
                 mov [si], al 
                 inc si
                 inc Bytes2Write
                 ;--moving each digit--
                 
                 dec bl
                 
                 cmp bl,0
                 jne prt_num2 
                 jmp end_pnu2
                 
       if_02:
                 mov dl,0h
                 mov ah,2
                 int 21h          
    
        end_pnu2:             
           pop dx
           pop cx
           pop bx
           pop ax
                
        
        
    ret
    int2ascii_uns endp
    
    ;*********************************************************************
    ; Int to Ascii signed - Convert a signed number to ascii chars  
    ; Input: None
    ; Output: None
    ; Destroys: CX
    ;*********************************************************************
     
    int2ascii proc
        
        push dx
        push ax
        
        mov ch, 0
        mov ax, cx
        ; the check SIGN of AX,
        ; make absolute if it's negative:
        cmp ax, 127
        jbe positive_ia
        neg ax
         
        push ax 
        ;PUTC    '-'
        
        mov [si],'-'
        inc si
        inc Bytes2Write
        
        pop ax
        
    positive_ia:
            call int2ascii_uns
    printed_ia:
            pop ax
            pop dx

        ret
    int2ascii endp

    ;************************************************************************
    ; Clear Char Routine - Clears the char the user inserted from the screen  
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;************************************************************************
    
    ClearChar proc
        
        push ax
        push dx
        
        mov dl, 08h
            
        mov ah, 2
        int 21h
                    
        mov dl, ' '
        mov ah, 2
        int 21h
            
        mov dl, 08h
            
        mov ah, 2
        int 21h
         
        pop dx
        pop ax
        
     ret
    ClearChar endp  

    ;***********************************************************************
    ; Exit Function Routine - Loads in the contents.bin the memory contents   
    ; Input: None
    ; Output: None
    ; Destroys: BX
    ;***********************************************************************    

    exit_func proc

        push ax
        push dx
        push cx
        
        mov al,1
        mov dx,offset path0
        call Fopen
            
        mov bx,ax
        mov cx,21
        mov dx, offset Matrix
        call Fwrite 
         
        call Fclose
        
        pop cx
        pop dx
        pop ax
    ret
    exit_func endp
    
    ;*****************************************************************************
    ; Contents Routine - Checks if contents.bin exists. If it doesn't, it creates. 
    ; Input: None
    ; Output: None
    ; Destroys: AL, BX, CX and DX
    ;*****************************************************************************
    
    Contents proc
       
        ;---Contents.bin management----    
        mov al,0
        mov dx,offset path0
        call Fopen
        jnc SkipCreation ; If there is a Contents.bin, the carry flag will be active and it will not create a new one
        
        mov ax,0  
        mov cx,0
        call Fcreate     ; Creates a Contents.bin. The file handle is an output to AX
        mov bx,ax
        
        call Fclose
        jmp ContentsBinEnd
        
      SkipCreation:  
        mov bx, ax
        mov cx,21
        mov dx, offset Matrix
        call Fread
        
        call Fclose
        
        ContentsBinEnd:
            ret
            
    Contents endp 
    
    ;************************************************************************************
    ; Clean Matrix Formula Routine - If a file exists and it's imported, we need to clean
    ;                              the matrix and the formula that was loaded before.  
    ; Input: None
    ; Output: None
    ; Destroys: Nothing
    ;************************************************************************************
  
    CleanMatrixFormula proc
       
       push bx
       
       mov bx, 0
       
       CleanMatrixCicle:
           mov Matrix[bx], 0h
           inc bx
           cmp bx, 16
           jne CleanMatrixCicle  
           mov bx, 0
           
       CleanFormula:
           mov FormulaBefore[bx], 0h 
           inc bx
           cmp bx, 5
           jne CleanFormula
               
       pop bx
       
    ret    
    CleanMatrixFormula endp 

    ;*********************************************************************
    ; Clean File Name Routine - cleans the filename string. 
    ; Input: DI = offset filename
    ; Output: None
    ; Destroys: DI
    ;*********************************************************************
     
    CleanFileName proc
        
        push cx
        
        mov cx, 0
        CleanFileNameCicle:
             mov [di], 0h
             inc di      
             inc cx
             cmp cx, 50
             jne CleanFileNameCicle   
       
       pop cx
        
    ret    
    CleanFileName endp
    
    ;**********************************************************************
    ; ReadNumFromImport Routine - Gets the cell value that is in BufString  
    ; Input:  BX = position of the BufString (BufString[bx]) 
    ;
    ; Output: BX = position of the BufString (BufString[bx])
    ;         AX = Number Read (Cell Value) from the BufString
    ;
    ; Destroys: AX and BX
    ;**********************************************************************   
    
    ReadNumFromImport proc 
         
       mov contador_alg,0
       mov FlagNotNumeric, 0 
       mov aux, 0   
       mov NegativeNumber, 0 
       
       ReadImport: 
           mov al,BufString[bx]     
           cmp contador_alg, 4
           je EndReadImport 
           inc contador_alg
           
           cmp BufString[bx], ';'
           je EndReadImport
           
           cmp al, 2Dh ; checks if the user entered '-' (negative number)
           je NegativeNumberCicle2 
           
           cmp al, 30h
           jb NotNumeric1
           
           cmp al, 39h 
           ja NotNumeric1
           
           sub al, 30h
           mov ah, 0h 
           push cx                       
           mov cx, ax     
           mov ax, aux
           push bx
           mov bx, 10   
           mul bx 
           pop bx
           mov aux, ax           
           add aux, cx
           pop cx
           inc bx
       jmp ReadImport
                   
       
       NegativeNumberCicle2:
          mov NegativeNumber, 1
          inc bx ;converte para negativo   
          jmp ReadImport  
          
       NotNumeric1:;lembrar de proteger
          mov FlagNotNumeric, 1 
       
       EndReadImport: 
       mov ax, aux
       cmp NegativeNumber, 1
       jne EndReadNum 
       
       NeedToNeg: 
            neg ax
            
       EndReadNum:
       inc bx 
                            
    ret
    ReadNumFromImport endp
    
    ;******************************************************************************************
    ; GetForOpRoutine - This routine gets the Operands and the Operation from the FormulaBefore 
    ; Input: None
    ; Output: Pos_1, Pos_2 and OpBefore.
    ; Destroys: Nothing
    ;******************************************************************************************
     
    GetForOp proc
        
        push ax
        push bx
        push cx
        
        mov bx, 0
        mov cx, 0
        
        GetForOpCicle:
        
            mov al, FormulaBefore[bx]
            
            cmp bx, 0
            je MustBeChar22
            
            cmp bx, 1
            je MustBeNum22 
            
            cmp bx, 2
            je MustBeOp22
                        
            cmp bx, 3
            je MustBeChar22
            
            cmp bx, 4
            je MustBeNum22 
            
            mov Pos_2, cl
            
            jmp GetForOpEnd 
            
        MustBeChar22: 
        
            cmp al, 'A'
            jne NextChar22
            jmp NextPosition 
            
        NextChar22:
            
            cmp al, 'a'
            jne NextChar33
            jmp NextPosition
            
        NextChar33: 
            
            cmp al, 'B'
            jne NextChar44
            inc cx
            
        NextChar44:
            
            cmp al, 'b'
            jne NextChar55
            inc cx
            jmp NextPosition
            
        NextChar55:
            
            cmp al, 'C'
            jne NextChar66
            add cx, 2
            jmp NextPosition
            
        NextChar66:
            
            cmp al, 'c'
            jne NextChar77
            add cx, 2
            jmp NextPosition
            
        NextChar77: ; must be D or d then.
            
            add cx, 3
            jmp NextPosition
            
        MustBeNum22:
            
            cmp al, '1'
            jne NextNum22
            jmp NextPosition
            
        NextNum22:
            
            cmp al, '2'
            jne NextNum33
            add cx, 4
            jmp NextPosition
            
        NextNum33:
            
            cmp al, '3'
            jne NextNum44 
            add cx, 8
            jmp NextPosition
            
            
        NextNum44: ; must be 4 then.
        
            add cx, 12
            jmp NextPosition
            
        MustBeOp22:
            
            mov OpBefore, al
          
       NextPosition:
       
            inc bx 
            
            cmp bx, 2
            je GetOp1
            jmp NotGetOp1
            
            GetOp1: 
            
                mov Pos_1, cl
                mov cx, 0
                
            NotGetOp1:
                
                jmp GetForOpCicle  
        
        
       GetForOpEnd: 
            mov bx, 0
            
            mov result_update, 1
            call ReadFormula
            mov result_update, 0 
            
            pop cx
            pop bx
            pop ax
             
     ret   
     GetForOp endp
                   
                 
ends

end start ; set entry point and stop the assembler.