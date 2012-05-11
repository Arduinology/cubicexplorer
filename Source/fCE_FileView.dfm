inherited CEFileViewPage: TCEFileViewPage
  Width = 586
  OnClick = View
  ExplicitWidth = 586
  object QuickViewSplitter: TSpTBXSplitter
    Left = 0
    Top = 0
    Height = 424
    Cursor = crSizeWE
    Visible = False
    ResizeStyle = rsPattern
    OnMoved = QuickViewSplitterMoved
  end
end
