Public Class Form1
    Dim pointList As New List(Of Point)
    Structure result
        Dim a
        Dim b
        Dim c
    End Structure



    Function getResult(a1, b1, c1, y1, a2, b2, c2, y2, a3, b3, c3, y3) As result
        Dim Dx As Single, Dy As Single, dd As Single, Dz As Single
        Dim a As Single, b As Single, c As Single, d As Single
        Dim e As Single, f As Single, g As Single, h As Single
        Dim i As Single, j As Single, k As Single, l As Single
        a = a1
        b = b1
        c = c1
        d = y1
        e = a2
        f = b2
        g = c2
        h = y2
        i = a3
        j = b3
        k = c3
        l = y3
        Dx = d * f * k + h * j * c + l * b * g - c * f * l - b * h * k - j * g * d
        Dy = a * h * k + e * l * c + d * g * i - c * h * i - g * l * a - d * e * k
        Dz = a * f * l + e * j * d + b * h * i - d * f * i - h * j * a - l * b * e
        dd = a * f * k + e * j * c + b * g * i - c * f * i - g * j * a - b * e * k
        Dim re As New result
        re.a = Dx / dd
        re.b = Dy / dd
        re.c = Dz / dd
        Return re
    End Function

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick

    End Sub
    Sub update()
        If IsNumeric(TextBox1.Text) And Not TextBox1.TextLength = 0 _
           And IsNumeric(TextBox2.Text) And Not TextBox2.TextLength = 0 _
           And IsNumeric(TextBox3.Text) And Not TextBox3.TextLength = 0 _
           And IsNumeric(TextBox4.Text) And Not TextBox4.TextLength = 0 _
           And IsNumeric(TextBox5.Text) And Not TextBox5.TextLength = 0 _
           And IsNumeric(TextBox6.Text) And Not TextBox6.TextLength = 0 Then

            Dim p1 As New Point(TextBox1.Text, TextBox2.Text), p2 As New Point(TextBox4.Text, TextBox3.Text), p3 As New Point(TextBox6.Text, TextBox5.Text)
            Dim x1 = p1.X, y1 = p1.Y, x2 = p2.X, y2 = p2.Y, x3 = p3.X, y3 = p3.Y
            Dim r As result = getResult(x1 * x1, x1, 1, y1, x2 * x2, x2, 1, y2, x3 * x3, x3, 1, y3)

            If r.a = 1 Then
                Label7.Text = "y=x^2"
            ElseIf r.a = -1 Then
                Label7.Text = "y=-x^2"
            Else
                Label7.Text = "y=" & r.a & "x^2"
            End If

            If r.b > 0 Then
                If r.b = 1 Then
                    Label7.Text = Label7.Text & "+"
                Else
                    Label7.Text = Label7.Text & "+" & r.b
                End If
            Else
                If r.b = -1 Then
                    Label7.Text = Label7.Text & "-"
                Else
                    Label7.Text = Label7.Text & r.b
                End If
            End If

            If r.c > 0 Then
                Label7.Text = Label7.Text & "x+" & r.c
            Else
                If r.c = 0 Then
                    Label7.Text = Label7.Text & "x"
                Else
                    Label7.Text = Label7.Text & "x" & r.c
                End If


            End If

            Label10.Text = "( " & r.b / (-2 * r.a) & " , " & (4 * r.a * r.c - r.b * r.b) / (4 * r.a) & " )"

            ' PictureBox2.Image = drawImg(r)

            If getDelta(r) > 0 Then
                Label13.Text = "( " & (-r.b + Math.Sqrt(getDelta(r))) / (2 * r.a) & " , " & 0 & " ) " & " ( " & (-r.b - Math.Sqrt(getDelta(r))) / (2 * r.a) & " , " & 0 & " )"
            ElseIf getDelta(r) = 0 Then

                Label13.Text = "( " & -r.b / 2 * r.a & " , " & 0 & " )"
            ElseIf getDelta(r) < 0 Then

                Label13.Text = "无交点"

            End If
            Debug.WriteLine(getDelta(r))
        End If
        GC.Collect()
    End Sub
    Function getDelta(r As result)
        Return r.b * r.b - 4 * r.a * r.c
    End Function
    Function drawImg(input As result)
        Dim r = input
        ' Dim sourceIMG As New Bitmap(PictureBox2.Width, PictureBox2.Height)
        ' Dim width = PictureBox2.Width - 2, height = PictureBox2.Height - 2
        ' Dim g As Graphics = Graphics.FromImage(sourceIMG)
        ' g.SmoothingMode = Drawing2D.SmoothingMode.AntiAlias
        ' g.DrawLine(Pens.Black, New Point(0, height / 2), New Point(width, height / 2))
        '  g.DrawLine(Pens.Black, New Point(width / 2, 0), New Point(width / 2, height))

        Dim scale = ((height / 2) - 10) / 2 / (4 * r.a * r.c - r.b * r.b) / (4 * r.a)
        ' Dim scalex = ((width / 2) - 10) / (4 * r.a * r.c - r.b * r.b) / (4 * r.a)
        For i = 10 To width - 10
            Dim i2 = i * scale
            Dim p1 = New Point(i, i2 * i2 * r.a + i2 * r.b + r.c)
            '  g.DrawLine(Pens.Black, New Point(p1.X - 1, p1.Y), p1)
            Debug.WriteLine("(" & p1.X & "," & p1.Y & ")")
        Next
        ' Return sourceIMG
    End Function

    Private Sub TextBox1_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox1.KeyDown
        If e.KeyCode = Keys.Enter Then
            TextBox2.Focus()
            TextBox2.SelectAll()
            update()
        End If
    End Sub
    Private Sub TextBox2_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox2.KeyDown
        If e.KeyCode = Keys.Enter Then
            TextBox4.Focus()
            TextBox4.SelectAll()
            update()
        End If
    End Sub
    Private Sub TextBox4_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox4.KeyDown
        If e.KeyCode = Keys.Enter Then
            TextBox3.Focus()
            TextBox3.SelectAll()
            update()
        End If
    End Sub
    Private Sub TextBox3_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox3.KeyDown
        If e.KeyCode = Keys.Enter Then
            TextBox6.Focus()
            TextBox6.SelectAll()
            update()
        End If
    End Sub
    Private Sub TextBox6_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox6.KeyDown
        If e.KeyCode = Keys.Enter Then
            TextBox5.Focus()
            TextBox5.SelectAll()
            update()
        End If
    End Sub
    Private Sub TextBox5_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox5.KeyDown
        If e.KeyCode = Keys.Enter Then
            update()
        End If
    End Sub

    Private Sub Label12_Click(sender As Object, e As EventArgs) Handles Label12.Click
        Me.Dispose()
    End Sub
End Class
