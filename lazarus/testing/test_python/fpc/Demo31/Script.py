
from spam import DelphiVersion, MainForm, DVar, CreateComponent, Application, Screen, mrOk, Form, Button, CheckBox, OpenDialog, caFree
from spam import Point, Monitor, DrawGrid, gdSelected, clBlue, ssCtrl, PageControl, TabSheet
if DelphiVersion >= 6:
  from spam import mdNearest
if DelphiVersion >= 15: 
  from spam import rtti_var

import unittest
import sys
py_major, py_minor = sys.version_info[:2]

class MyForm(Form):
  def __init__(self, Owner):
    self.Caption = 'Subclassed form'
    self.btnClose = Button(self)
    self.btnClose.Parent = self
    self.btnClose.Caption = 'Close'
    self.btnClose.SetBounds(10, 10, 120, 30)
    self.btnClose.OnClick = self.btnCloseClick

    self.chkCanClose = CheckBox(self)
    self.chkCanClose.Parent = self
    self.chkCanClose.Caption = 'Can close?'
    self.chkCanClose.SetBounds(10, 50, 120, 30)

    self.grdTest = DrawGrid(self)
    self.grdTest.Parent = self
    self.grdTest.SetBounds(10, 100, 300, 250)
    self.grdTest.OnDrawCell = self.grdTestDrawCell
    self.grdTest.OnSelectCell = self.grdTestSelectCell

    self.OnCloseQuery = self.MyFormCloseQuery
    self.OnClose = self.MyFormClose
    self.Width = 400
    self.Height = 400

  def btnCloseClick(self, Sender):
    print("Close!")
    self.Close()

  def MyFormCloseQuery(self, Sender, CanClose):
    CanClose.Value = self.chkCanClose.Checked

  def MyFormClose(self, Sender, Action):
    Action.Value = caFree

  def grdTestDrawCell(self, Sender, Col, Row, Rect, State):
    if gdSelected in State:
      Sender.Canvas.Brush.Color = clBlue # 0x00ff0000 # blue
      print("Cell[%d, %d] is selected, Rect=%s, State=%s" % (Col, Row, Rect, State))
    Sender.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, "%d @ %d" % (Col, Row))

  def grdTestSelectCell(self, Sender, Col, Row, CanSelect):
    if Col == 2 and Row == 2:
      CanSelect.Value = False

class TTestForm(Form):
  def __init__(self, Owner):
    self.Caption = self.Caption + ' - changed by Python subclass'
    self.BindMethodsToEvents()

  def handle_btnAdd_OnClick(self, Sender):
    self.ListBox1.Items.Add(self.Edit1.Text)

class TestDelphiWrapper(unittest.TestCase):

    def testReadWriteProperties(self):
        DVar.SValue = 'Weight'
        self.assertEqual(DVar.SValue, 'Weight')
        DVar.IValue = 70
        self.assertEqual(DVar.IValue, 70)
        MainForm.Caption = 'PyDelphi rocks!'  #setting properties
        self.assertEqual(MainForm.Caption, 'PyDelphi rocks!')
        print()
        print('MainForm.ActiveControl=',  MainForm.ActiveControl)  # class properties
        MainForm.BorderStyle = 'bsSizeable' #enumeration property
        MainForm.Anchors = ['akLeft', 'akTop'] #set property
        self.assertEqual(MainForm.Anchors, ['akLeft', 'akTop'])

    def testTObject(self):
        self.assertEqual(MainForm.ClassName, 'TForm1')
        self.assertTrue(MainForm.InheritsFrom('TObject'))

    def testMethodCall(self):
        if DelphiVersion >= 7:
            DVar.SetMeUp('Age', 25)
            self.assertEqual(DVar.SValue, 'Age')
            self.assertEqual(DVar.IValue, 25)
            print()
            print(DVar.DescribeMe()) #method calls

    def testRepr(self):
        print()
        print('Representation of Delphi objects')
        print(DVar)
        print(MainForm)
        if DelphiVersion >= 7:
            print(DVar.DescribeMe)  # method object
        print(DVar.SL) # TStrings

    def testTStrings(self):
        SL = DVar.SL
        self.assertEqual(len(SL), 2)
        print()
        for i in DVar.SL: print(i, ' contains ', DVar.SL[i])
        SL.Add('New String')
        self.assertEqual(len(SL), 3)
        self.assertEqual(SL.IndexOf('New String'), 2)
        self.assertEqual(SL[0], 'Form1')
        self.assertEqual(SL.Objects[0], MainForm)
        self.assertEqual(MainForm in SL.Objects, True)
        SL[2] = 'Changed'
        self.assertEqual(SL[2], 'Changed')
        self.assertEqual(SL.Objects[2], None)
        SL.Objects[2] = MainForm
        self.assertEqual(SL.Objects[2], MainForm)
        self.assertEqual(MainForm in SL.Objects, True)
        SL.Delete(2)
        self.assertEqual(len(SL), 2)
        print("str(SL) =", str(SL))
        SL.Assign([1, 2, 3])
        self.assertEqual(len(SL), 3)
        self.assertEqual('2' in SL, True)
        self.assertEqual(SL[1], '2')
        tmp = ['a', 'b', 'c']
        SL.Assign(tmp)
        self.assertEqual(SL.ToList(), tmp)
        tmp = ('a', 'b', 'c')
        SL.Assign(tmp)
        self.assertEqual(SL.ToTuple(), tmp)

    def testTComponent(self):
        self.assertEqual(MainForm.Name, 'Form1')
        count = MainForm.ComponentCount
        def SetComponentCount():
            MainForm.ComponentCount = 3
        if py_major == 2 and py_minor < 5:
            self.assertRaises(TypeError, SetComponentCount)
        else:
            self.assertRaises(AttributeError, SetComponentCount)
        self.assertEqual(MainForm.__owned__, False)

    def testSubComponents(self):
        MainForm.Button1.Caption = 'Click me!!'
        self.assertEqual(MainForm.Button1.Caption, 'Click me!!')
       # test alternate mapping notation
        MainForm['Button1'].Caption = 'Click me!!!!'
        self.assertEqual(MainForm['Button1'].Caption, 'Click me!!!!')
        self.assertEqual(MainForm.Button1.Caption, 'Click me!!!!')
        # Test Owner property and DelphiObject comparison
        self.assertTrue(MainForm.Button1.Owner == MainForm)
        print([i.Name for i in MainForm.Components])

    def testCreateComponent(self):
        NewButton = CreateComponent('TButton', None)
        self.assertEqual(NewButton.__bound__, True)
        self.assertEqual(NewButton.__owned__, True)
        NewButton.Free()
        self.assertEqual(NewButton.__bound__, False)

    def testWinControls(self):
        self.assertEqual(MainForm.Panel1.Parent, MainForm)
        print()
        print('MainForm contains ', MainForm.ControlCount, ' controls')
        print([i.Name for i in MainForm.Controls])

    def testForm(self):
        MyForm = CreateComponent('TForm', None)
        MyForm.Name = 'MyForm'
        MyForm.Caption = 'Python Generated Form'
        MyForm.Height = 300
        MyForm.Width = 310
        MyForm.Position = 'poDefaultPosOnly'
        PC = PageControl(MyForm)
        PC.Name = "MyPageControl"
        PC.Parent = MyForm
        PC.Align = "alClient"
        P1 = TabSheet(MyForm)
        P1.PageControl = PC
        P1.Caption = "Page 1"
        LB = CreateComponent('TListBox', MyForm)
        B1 = CreateComponent('TButton', MyForm)
        B2 = CreateComponent('TButton', MyForm)
        Edit = CreateComponent('TEdit', MyForm)
        LB.Name = 'ListBox1'
        LB.Parent = P1
        LB.Left = 20
        LB.Top = 14
        LB.Width = 121
        LB.Height = 97
        LB.TabOrder = 0
        B1.Name = 'Button1'
        B1.Parent = P1
        B1.Left = 185
        B1.Top = 60
        B1.Width = 75
        B1.Height = 25
        B1.Caption = 'Add to list'
        B1.TabOrder = 1
        Edit.Name = 'Edit1'
        Edit.Parent = P1
        Edit.Left = 168
        Edit.Top = 27
        Edit.Width = 121
        Edit.Height = 21
        Edit.TabOrder = 2
        Edit.Text = 'Add me to List'
        B2.Name = 'Button2'
        B2.Parent = P1
        B2.Left = 121
        B2.Top = 125
        B2.Width = 75
        B2.Height = 25
        B2.Caption = 'Close'
        B2.ModalResult = mrOk
        B2.TabOrder = 3
        def ClickHandler(Sender):
            print(Sender.Name, ' was clicked')
            LB.Items.Add(Edit.Text)
        B1.OnClick = ClickHandler
        def KeyPressHandler(Sender, Key):
           if Key.Value == 'a':
              Key.Value = 'z'
        Edit.OnKeyPress = KeyPressHandler
        def KeyDownHandler(Sender, Key, Shift):
           # forbid Ctrl+Home
           if Key.Value == 36 and ssCtrl in Shift:
              Key.Value = 0
        Edit.OnKeyDown = KeyDownHandler
        BtnNextPage = Button(MyForm)
        BtnNextPage.Parent = P1
        BtnNextPage.SetBounds(10, 200, 150, 24)
        BtnNextPage.Caption = "Select Next Page"
        def NextPageClick(Sender):
           Sender.Owner.MyPageControl.SelectNextPage(True, True)
        BtnNextPage.OnClick = NextPageClick
        P2 = TabSheet(MyForm)
        P2.PageControl = PC
        P2.Caption = "Page 2"
        BtnGotoPage3 = Button(MyForm)
        BtnGotoPage3.Parent = P2
        BtnGotoPage3.SetBounds(10, 10, 150, 24)
        BtnGotoPage3.Caption = "Goto Page 3"
        def GotoPage3Click(Sender):
           Sender.Owner.MyPageControl.ActivePage = Sender.Owner.Page3
        BtnGotoPage3.OnClick = GotoPage3Click

        P3 = TabSheet(MyForm)
        P3.PageControl = PC
        P3.Caption = "Page 3"
        P3.Name = "Page3"
        BtnGotoFirstPage = Button(MyForm)
        BtnGotoFirstPage.Parent = P3
        BtnGotoFirstPage.SetBounds(10, 10, 150, 24)
        BtnGotoFirstPage.Caption = "Goto First Page"
        def GotoFirstPageClick(Sender):
           PC.ActivePageIndex = 0
        BtnGotoFirstPage.OnClick = GotoFirstPageClick
        chkAllowChange = CheckBox(MyForm)
        chkAllowChange.Parent = P3
        chkAllowChange.SetBounds(10, 40, 150, 24)
        chkAllowChange.Caption = "Allow Page Change?"
        chkAllowChange.Checked = True

        def PCChanging(Sender, AllowChange):
           AllowChange.Value = chkAllowChange.Checked
        PC.OnChanging = PCChanging

        MyForm.ShowModal()

    def testFormSubclass(self):
        f = MyForm(Application)
        try:
          print(f.ShowModal())
        finally:
          f.Free()

    def testFormSubclass2(self):
        f = TTestForm(Application)
        try:
          print(f.ShowModal())
        finally:
          f.Free()

    def testPointConversions(self):
      p1 = Point(10, 10)
      p = MainForm.ClientToScreen(p1)
      print(p)
      p2 = MainForm.ScreenToClient(p)
      self.assertEqual(p2.X, p1.X)
      self.assertEqual(p2.Y, p1.Y)

    def testObjectNotification(self):
        DVar.IValue = 0
        def ChangeHandler(Sender):
            print(Sender)
            Sender.IValue = 55
        DVar.OnChange = ChangeHandler
        if DelphiVersion >= 7:
            DVar.TriggerChange()
            self.assertEqual(DVar.IValue, 55)
            DVar.OnChange = None
            DVar.IValue = 0
            DVar.TriggerChange()
            self.assertEqual(DVar.IValue, 0)

    def testActions(self):
        self.assertEqual(MainForm.ActionList1.ActionCount, 1)
        self.assertEqual(MainForm.ActionList1.Actions[0], MainForm.actTest)
        self.assertEqual(MainForm.actTest in MainForm.ActionList1, True)
        DVar.IValue = 0
        MainForm.actTest.Execute()
        self.assertEqual(DVar.IValue, 1)
        def ActionHandler(Sender):
            print("Action", Sender.Name, "executed from Python")
            DVar.IValue = 2
        self.assertEqual(MainForm.actTest.OnExecute, None)
        MainForm.actTest.OnExecute = ActionHandler
        self.assertEqual(MainForm.actTest.OnExecute, ActionHandler)
        DVar.IValue = 0
        MainForm.actTest.Execute()
        self.assertEqual(DVar.IValue, 2)
        MainForm.actTest.OnExecute = None
        self.assertEqual(MainForm.actTest.OnExecute, None)

    def testScreen(self):
        self.assertEqual(Screen.DataModuleCount, 0)
        self.assertEqual(Screen.FormCount > 0, True)
        self.assertEqual(Screen.CustomFormCount > 0, True)
        idx = -1
        for i, f in enumerate(Screen.Forms):
          if f == MainForm:
            idx = i
            break;
        self.assertEqual(idx > -1, True)
        self.assertEqual(Screen.Forms[idx], MainForm)
        self.assertEqual(Screen.CustomForms[idx], MainForm)
        self.assertEqual(Screen.Width > 0, True)
        self.assertEqual(Screen.Height > 0, True)
        MainForm.Button1.SetFocus()
        self.assertEqual(Screen.ActiveControl, MainForm.Button1)
        def ActiveControlChangeHandler(Sender):
            print("ActiveControlChangeHandler fired")
            print("New active constrol is", Sender.ActiveControl.Name)
            DVar.IValue = 2
        self.assertEqual(Screen.OnActiveControlChange, None)
        Screen.OnActiveControlChange = ActiveControlChangeHandler
        self.assertEqual(Screen.OnActiveControlChange, ActiveControlChangeHandler)
        DVar.IValue = 0
        self.assertEqual(DVar.IValue, 0)
        MainForm.Memo2.SetFocus()
        self.assertEqual(DVar.IValue, 2)
        Screen.OnActiveControlChange = None
        self.assertEqual(Screen.OnActiveControlChange, None)
        DVar.IValue = 0
        MainForm.Button1.SetFocus()
        self.assertEqual(DVar.IValue, 0)
        if DelphiVersion >= 6:
            m = Screen.MonitorFromPoint(Point(10, 10), mdNearest)
            self.assertEqual(isinstance(m, Monitor), True)

    def testOpenDialog(self):
        print("OpenDialog test:")
        open_dialog = CreateComponent("TOpenDialog", None)
        if open_dialog.Execute():
            print("FileName:", open_dialog.FileName)
        open_dialog.Free()

    def testClassProperty(self):
        MainForm.ActiveControl = MainForm.Memo2 # Class property!
        self.assertEqual(MainForm.ActiveControl, MainForm.Memo2)
    
    def testDir(self):
        L = dir(MainForm)
        self.assertTrue('ComponentCount' in L)
        self.assertTrue('__owned__' in L)
        self.assertTrue('ShowModal' in L)

    def testRttiAccess(self):
        if DelphiVersion >= 15:
          rtti_var.Fruit = 'Apple'
          self.assertTrue(rtti_var.Fruit == 'Apple')
          rtti_var.Fruits = ['Apple', 'Banana']
          self.assertTrue(rtti_var.Fruits == ['Apple', 'Banana'])
          rtti_var.BuyFruits(['Apple', 'Orange'])
          self.assertTrue(rtti_var.Fruits == ['Apple', 'Orange'])
          rtti_var.SetFormCaption(MainForm, 'From TTestRTTIAccess')
          self.assertEqual(MainForm.Caption, 'From TTestRTTIAccess')
          rtti_var.FruitField = 'Apple'
          self.assertTrue(rtti_var.FruitField == 'Apple')
          rtti_var.FruitsField = ['Apple', 'Banana']
          self.assertTrue(rtti_var.FruitsField == ['Apple', 'Banana'])
          rtti_var.StringField = 'Hi'
          self.assertTrue(rtti_var.StringField == 'Hi')
          rtti_var.DoubleField = 3.14
          self.assertTrue(rtti_var.DoubleField == 3.14)
          rtti_var.ObjectField = MainForm
          MainForm.Caption = 'From TTestRTTIAccess.ObjectField'
          self.assertTrue(MainForm.Caption == 'From TTestRTTIAccess.ObjectField')


if __name__ == '__main__':
    try:
        unittest.main()
    except SystemExit:
        pass

		
		