Public Class Principal
    Dim posicion_cabezal As Integer = 1
    Dim valor_posicion As String

    Dim proximo_estado As String
    Dim estado_actual As String 'Uso en un futuro

    Dim mensaje_error As String = "Machine Halted"
    Dim desplazamiento As String
    Dim cuadro(13) As String
    Dim aceptado As Boolean
    Dim fila As Integer = 0

    Dim Pila As New VBstack


    Private Sub Principal_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Cargar_Cuadros()

    End Sub

    'ACTUALIZACIÓN
    Public Sub Cargar_Cuadros()
        cuadro(0) = TextBox0.Text
        cuadro(1) = TextBox1.Text
        cuadro(2) = TextBox2.Text
        cuadro(3) = TextBox3.Text
        cuadro(4) = TextBox4.Text
        cuadro(5) = TextBox5.Text
        cuadro(6) = TextBox6.Text
        cuadro(7) = TextBox7.Text
        cuadro(8) = TextBox8.Text
        cuadro(9) = TextBox9.Text
        cuadro(10) = TextBox10.Text
        cuadro(11) = TextBox11.Text
        cuadro(12) = TextBox12.Text
        cuadro(13) = TextBox13.Text
    End Sub
    Public Sub Actualizar_Textboxs()
        TextBox0.Text = cuadro(0)
        TextBox1.Text = cuadro(1)
        TextBox2.Text = cuadro(2)
        TextBox3.Text = cuadro(3)
        TextBox4.Text = cuadro(4)
        TextBox5.Text = cuadro(5)
        TextBox6.Text = cuadro(6)
        TextBox7.Text = cuadro(7)
        TextBox8.Text = cuadro(8)
        TextBox9.Text = cuadro(9)
        TextBox10.Text = cuadro(10)
        TextBox11.Text = cuadro(11)
        TextBox12.Text = cuadro(12)
        TextBox13.Text = cuadro(13)
    End Sub

    'RB
    Private Sub ControlGraficos()
        If grafoRB.Checked = True Then
            grafoPB.Visible = True
            TransicionesGB.Visible = False
        Else
            grafoPB.Visible = False
            TransicionesGB.Visible = True
        End If
    End Sub

    'SET UP
    Private Sub SetUp()
        posicion_cabezal = 1
        valor_posicion = cuadro(1)
        proximo_estado = "q0"
        estado_actual = "q0"
        desplazamiento = "+"

    End Sub
    'MOVIMIENTOS
    Public Sub muevete_a_la_derecha()
        posicion_cabezal += 1
        valor_posicion = cuadro(posicion_cabezal)
        desplazamiento = "+"
    End Sub
    Public Sub muevete_a_la_izquierda()
        posicion_cabezal -= 1
        valor_posicion = cuadro(posicion_cabezal)
        desplazamiento = "-"
    End Sub
    Public Sub no_te_muevas()
        posicion_cabezal = posicion_cabezal
        valor_posicion = cuadro(posicion_cabezal)
        desplazamiento = "="
    End Sub
    '***************************************
    'ESCRITURA
    Public Sub escribe_0()
        valor_posicion = "0"
        cuadro(posicion_cabezal) = valor_posicion
    End Sub
    Public Sub escribe_1()
        valor_posicion = "1"
        cuadro(posicion_cabezal) = valor_posicion
    End Sub
    Public Sub escribe_c()
        valor_posicion = "c"
        cuadro(posicion_cabezal) = valor_posicion
    End Sub
    Public Sub escribe_u()
        valor_posicion = "u"
        cuadro(posicion_cabezal) = valor_posicion
    End Sub
    Public Sub no_escribas()
        valor_posicion = valor_posicion
    End Sub
    '***************************************
    'ESTADOS
    Private Sub q0()
        Select Case valor_posicion
            Case "0"
                escribe_c()
                muevete_a_la_derecha()
                proximo_estado = "q1"
            Case "1"
                escribe_u()
                muevete_a_la_derecha()
                proximo_estado = "q2"
            Case Else
                MsgBox(mensaje_error, MsgBoxStyle.Critical)
        End Select
    End Sub
    Private Sub q1()
        Select Case valor_posicion
            Case "0", "1", "c", "u"
                no_escribas()
                muevete_a_la_derecha()
                proximo_estado = "q1"
            Case " "
                escribe_u()
                muevete_a_la_izquierda()
                proximo_estado = "q3"
            Case Else
                MsgBox(mensaje_error, MsgBoxStyle.Critical)
        End Select
    End Sub
    Private Sub q2()
        Select Case valor_posicion
            Case "0", "1", "c", "u"
                no_escribas()
                muevete_a_la_derecha()
                proximo_estado = "q2"
            Case " "
                escribe_c()
                muevete_a_la_izquierda()
                proximo_estado = "q3"
            Case Else
                MsgBox(mensaje_error, MsgBoxStyle.Critical)
        End Select
    End Sub
    Private Sub q3()
        Select Case valor_posicion
            Case "c", "u"
                no_escribas()
                muevete_a_la_izquierda()
                proximo_estado = "q3"
            Case "0", "1"
                no_escribas()
                muevete_a_la_izquierda()
                proximo_estado = "q4"
            Case " "
                no_escribas()
                muevete_a_la_derecha()
                proximo_estado = "q5"
            Case Else
                MsgBox(mensaje_error, MsgBoxStyle.Critical)
        End Select
    End Sub
    Private Sub q4()
        Select Case valor_posicion
            Case "0", "1"
                no_escribas()
                muevete_a_la_izquierda()
                proximo_estado = "q4"
            Case "c", "u"
                no_escribas()
                muevete_a_la_derecha()
                proximo_estado = "q0"
            Case Else
                MsgBox(mensaje_error, MsgBoxStyle.Critical)
        End Select
    End Sub
    Private Sub q5()
        Select Case valor_posicion
            Case "c"
                escribe_0()
                muevete_a_la_derecha()
                proximo_estado = "q5"
            Case "u"
                escribe_1()
                muevete_a_la_derecha()
                proximo_estado = "q5"
            Case " "
                no_escribas()
                muevete_a_la_izquierda()
                proximo_estado = "qF"

        End Select
    End Sub


    '"BRAZO" DEL AUTÓMATA
    Private Sub Manejador_Estados()
        Select Case proximo_estado
            Case "q0"
                q0()
            Case "q1"
                q1()
            Case "q2"
                q2()
            Case "q3"
                q3()
            Case "q4"
                q4()
            Case "q5"
                q5()


        End Select
    End Sub


    Private Sub playBT_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles playBT.Click
        Cargar_Cuadros()
        SetUp()
        DataGridView1.Rows.Insert(fila, posicion_cabezal, valor_posicion, desplazamiento, estado_actual, proximo_estado)
        Do Until proximo_estado = "qF"
            estado_actual = proximo_estado
            Manejador_Estados()
            Actualizar_Textboxs()
            fila += 1
            DataGridView1.Rows.Insert(fila, posicion_cabezal, valor_posicion, desplazamiento, estado_actual, proximo_estado)
        Loop
        MsgBox("Recorrido Terminado")
    End Sub

    'CONTROL sobre la entrada
    'Solo 1 y 0s
    Private Sub KeyControl(ByVal valor)
        If Char.IsControl(valor) = False Then
            If valor <> "1" And valor <> "0" Then
                aceptado = True
            Else
                aceptado = False
            End If
        End If
    End Sub
    Private Sub TextBox1_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox1.KeyPress
        KeyControl(e.KeyChar)
        e.Handled = aceptado
    End Sub
    Private Sub TextBox2_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox2.KeyPress
        KeyControl(e.KeyChar)
        e.Handled = aceptado
    End Sub
    Private Sub TextBox3_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox3.KeyPress
        KeyControl(e.KeyChar)
        e.Handled = aceptado
    End Sub
    Private Sub TextBox4_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox4.KeyPress
        KeyControl(e.KeyChar)
        e.Handled = aceptado
    End Sub
    Private Sub TextBox5_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox5.KeyPress
        KeyControl(e.KeyChar)
        e.Handled = aceptado
    End Sub
    Private Sub TextBox6_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox6.KeyPress
        KeyControl(e.KeyChar)
        e.Handled = aceptado
    End Sub

    'Solo 1 TBX disponible a la vez
    Private Sub TextBox1_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyUp
        If TextBox1.Text <> "" And TextBox1.Text <> " " Then
            TextBox1.ReadOnly = True
            TextBox2.ReadOnly = False
        End If
    End Sub
    Private Sub TextBox2_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox2.KeyUp
        If TextBox2.Text <> "" And TextBox2.Text <> " " Then
            TextBox2.ReadOnly = True
            TextBox3.ReadOnly = False
        End If
    End Sub
    Private Sub TextBox3_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox3.KeyUp
        If TextBox3.Text <> "" And TextBox3.Text <> " " Then
            TextBox3.ReadOnly = True
            TextBox4.ReadOnly = False
        End If
    End Sub
    Private Sub TextBox4_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox4.KeyUp
        If TextBox4.Text <> "" And TextBox4.Text <> " " Then
            TextBox4.ReadOnly = True
            TextBox5.ReadOnly = False
        End If
    End Sub
    Private Sub TextBox5_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox5.KeyUp
        If TextBox5.Text <> "" And TextBox5.Text <> " " Then
            TextBox5.ReadOnly = True
            TextBox6.ReadOnly = False
        End If
    End Sub
    Private Sub TextBox6_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TextBox6.KeyUp
        If TextBox6.Text <> "" And TextBox6.Text <> " " Then
            TextBox6.ReadOnly = True
        End If
    End Sub

    Private Sub grafoRB_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles grafoRB.CheckedChanged
        ControlGraficos()
    End Sub

    Private Sub tablaRB_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tablaRB.CheckedChanged
        ControlGraficos()
    End Sub

    Private Sub TransicionesGB_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TransicionesGB.Enter

    End Sub

    Private Sub TextBox61_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox61.TextChanged

    End Sub

    Private Sub TextBox60_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox60.TextChanged

    End Sub

    Private Sub grafoPB_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles grafoPB.Click

    End Sub
End Class
