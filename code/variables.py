v1 = """SEQ ceq TXDITC  TXDB ITCB PSTKRV PSTKL PSTK prcc_f csho epsfx epsfi oprepsx opeps ajex ebit spi nopi
  sale ibadj dvc dvp ib oibdp dp oiadp gp revt cogs pi ibc dpc at ni ibcom icapt mib ebitda xsga
  xido xint mii ppent act lct dltt dlc che invt lt rect xopr oancf txp txt ap xrd xad xlr capx"""

v1 = v1.lower()

v1 = v1.split()

v = [x+'q' for x in v1]
