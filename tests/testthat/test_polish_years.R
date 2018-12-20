context("Polish years")

test_that("Year polishing is correct", {

  # Limits
  #expect_true(!is.na(polish_years("19-19")$from))
  #expect_true(!is.na(polish_years("19-19")$till))

  expect_true(is.na(polish_years("19-19", min.year = 1000)$from))
  expect_true(is.na(polish_years("19-19", min.year = 1000)$till))

  expect_true(is.na(polish_years("19920831-19920831")$from))
  expect_true(is.na(polish_years("19920831-19920831")$till))

  expect_true(is.na(polish_years("19765-19765")$from))
  expect_true(is.na(polish_years("19765-19765")$till))
  
  expect_true(is.na(polish_years("20021")$from))
  expect_true(is.na(polish_years("20021")$till))

  expect_equal(polish_years("6-1939", min.year = 1000)$from, 1939)
  expect_true(is.na(polish_years("6-1939", min.year = 1900)$till)  )

  # -------------------------------------------------------

  expect_equal(polish_years("1741-1810.")$from, 1741)    
  expect_equal(polish_years("1741-1810.")$till, 1810)
  expect_equal(polish_years("1617 [] 1618")$from, 1617)
  expect_equal(polish_years("1617 [] 1618")$till, 1618)    

  expect_equal(polish_years("1798. (price one dollar)")$from, 1798)
  expect_equal(polish_years("1776.")$from, 1776)  
  expect_equal(polish_years("[1768.]")$from, 1768)
  expect_equal(polish_years("[-1768]")$till, 1768)

  # Fix later
  expect_true(is.na(polish_years("-1776.")$from))
  expect_true(is.na(polish_years("18th century")$from))
  
  expect_equal(polish_years("-1768")$till, 1768)      
  expect_equal(polish_years("-1776.")$till, 1776)

  expect_equal(polish_years("1524]")$from, 1524)
  expect_equal(polish_years("1524?]")$from, 1524)
  expect_equal(polish_years("[1524?]")$from, 1524)
  expect_equal(polish_years("--1524.---")$from, 1524)
  expect_equal(polish_years("[ca. 1618]")$from, 1618)  

  expect_equal(polish_years("MDCCLXVIII. [1768]")$from, 1768)
  expect_equal(polish_years("MDCCLXVIII. 1768")$from, 1768)

  expect_true(is.na(polish_years("active 1781.")$till))

  expect_equal(polish_years("1741?-1821.")$from, 1741)
  expect_equal(polish_years("1741?-1821.")$till, 1821)

  expect_equal(polish_years("1650 or 1651-1705.")$from, 1651)
  expect_equal(polish_years("1650 or 1651-1705.")$till, 1705)
  expect_equal(polish_years("[1524-1528]")$from, 1524)
  expect_equal(polish_years("[1524-1528]")$till, 1528)

  expect_true(is.na(polish_years("active 17th century.")$from))
  expect_true(is.na(polish_years("active 17th century.")$till))
  expect_true(is.na(polish_years("17th century")$from))  

  expect_equal(polish_years("[between 1790 and 1800?]")$from, 1790)
  expect_equal(polish_years("[between 1790 and 1800?]")$till, 1800)
  expect_equal(polish_years("1700/1.")$from, 1700)
  expect_true(is.na(polish_years("1700/1.")$till))
  expect_equal(polish_years("MDCXLI. [1641, i.e. 1642]")$from, 1642)
  expect_true(is.na(polish_years("MDCXLI. [1641, i.e. 1642]")$till))
  expect_equal(polish_years("1670/71.]")$from, 1670)
  expect_equal(polish_years("re-printed in the year, 1689]")$from, 1689)
  expect_equal(polish_years("M.DC.XXVII [1627, i.e. 1628]")$from, 1628)
  expect_equal(polish_years("Re-printed in the year, 1689]")$from, 1689)
  expect_equal(polish_years("August 5, 1799.")$from, 1799)
  expect_equal(polish_years("1726[1727]")$from, 1727)
  expect_equal(polish_years("1726[7]")$from, 1727)
  
  expect_equal(polish_years("May 27, 1643.")$from, 1643)
  expect_equal(polish_years("Iune 8, 1642.")$from, 1642)
  expect_equal(polish_years("Anno M.DC.XVIII [1618, i.e. 1619]")$from, 1619)
  expect_equal(polish_years("27 Octob. 1643.")$from, 1643)
  expect_equal(polish_years("1797-98.")$from, 1797)
  expect_equal(polish_years("1797-98.")$till, 1798)
  expect_equal(polish_years("re-printed; 1688.")$from, 1688)
  expect_equal(polish_years("Re-printed in the year. [sic, i.e. 1680?]")$from, 1680)
  expect_equal(polish_years("re-printed by George Mosman,;1700.")$from, 1700)
  expect_equal(polish_years("Printed on the day of Jacobs trouble, and to make way (in hope) for his deliverance out of it. May 25. 1643.")$from, 1643)
  expect_equal(polish_years("Printed July 17, in the yeer 1648.")$from, 1648)
  expect_equal(polish_years("Printed in the Yeare,;1648.")$from, 1648)
  expect_equal(polish_years("Anno. M.D.XXXVIII [1538, i.e. ca. 1562]")$from, 1562)
  expect_equal(polish_years("&lt;1776?>")$from, 1776)
  expect_equal(polish_years("&lt;1766- >")$from, 1766)
  expect_equal(polish_years("--March--1797--")$from, 1797)
  expect_equal(polish_years("Prändätty tänä wuonna [1764]")$from, 1764)
  expect_equal(polish_years("prändätty wuonna 1690,;1690.")$from, 1690)
  expect_equal(polish_years("1893[-95]")$from, 1893)
  expect_equal(polish_years("1893[-95]")$till, 1895)      
  expect_equal(polish_years("[1917];[1917]")$from, 1917)
  expect_equal(polish_years("[1915?]-[1916?]")$from, 1915)
  expect_equal(polish_years("[1915?]-[1916?]")$till, 1916)  
  expect_equal(polish_years("[1913]-19??")$from, 1913)
  expect_true(is.na(polish_years("[1913]-19??")$till))
  expect_equal(polish_years("[1908}")$from, 1908)
  expect_equal(polish_years("[1908] (painettu osakeyhtiö F. Tilgmannilla)")$from, 1908)
  expect_equal(polish_years("[1899]-[1899]")$from, 1899)
  expect_equal(polish_years("[1899]-[1899]")$till, 1899)   
  expect_equal(polish_years("[1897-]-1898.")$from, 1897)
  expect_equal(polish_years("[1897-]-1898.")$till, 1898)
  expect_equal(polish_years("[1801],;1801.")$from, 1801)
  expect_equal(polish_years("[1784];4:o.")$from, 1784)
  expect_equal(polish_years("[1768],;2:o.")$from, 1768)  
  expect_equal(polish_years("[1686],;1686.")$from, 1686)  
  expect_equal(polish_years("[1680],;1680.")$from, 1680)  
  expect_equal(polish_years("[1675] Turku,;[1:o?]")$from, 1675)  
  expect_equal(polish_years("[1672] Strängnäs.")$from, 1672)
  expect_equal(polish_years("1906-[27]")$from, 1906)
  expect_equal(polish_years("1906-[27]")$till, 1927) 
  expect_equal(polish_years("anno Dni 1642. 16 aprilis.")$from, 1642)
  expect_equal(polish_years("anno 1631 [po. 1632]")$from, 1632)
  expect_equal(polish_years("anno 1622. 4. Octob.")$from, 1622)
  expect_equal(polish_years("anno 1622. 25. Ian.")$from, 1622)
  expect_equal(polish_years("anno 1599 die 15. Februarii.")$from, 1599)
  expect_equal(polish_years("1916-[19--?].")$from, 1916)
  expect_equal(polish_years("1915-[19??].")$from, 1915)
  expect_equal(polish_years("1906-[19??]")$from, 1906)
  expect_equal(polish_years("1905 [po. 1906]")$from, 1906)
  expect_equal(polish_years("1901 [i. e. 1900]")$from, 1900)
  expect_equal(polish_years("1896-[19--].")$from, 1896)
  expect_equal(polish_years("1757 [po. 1751]")$from, 1751)
  expect_equal(polish_years("1738 [po. 1752].")$from, 1752)
  expect_equal(polish_years("[anno Dni 1544. Nouembris 12]")$from, 1544)
  expect_equal(polish_years("[anno 1575 septembris 12]")$from, 1575)
  expect_equal(polish_years("[a:1680]")$from, 1680)
  expect_equal(polish_years("[1955?]-[19--?]")$from, 1955)
  expect_equal(polish_years("[1950?]-19??")$from, 1950)
  expect_equal(polish_years("[1900?]-[190-?]")$from, 1900)
  expect_equal(polish_years("[19--?]-1913?")$till, 1913)
  expect_equal(polish_years("[1896?]-[19--?].")$from, 1896)

  expect_equal(polish_years("[1899-]1900.")$from, 1899)
  expect_equal(polish_years("[1899-]1900.")$till, 1900)

  expect_equal(polish_years("1641. [1642]")$from, 1642)
  expect_equal(polish_years("1677. [i.e. 1689.].")$from, 1689)

  expect_equal(polish_years("Printed in the year of the truely-hoped-for reformation of Englands oppressions and horrid deformation. 1647.")$from, 1647)
  expect_equal(polish_years("Printed in the Year MDCLXXX.")$from, 1680)
  expect_equal(polish_years("Printed in the first yeare of Jubilee,1643.")$from, 1643)

  expect_equal(polish_years("MDCCX.-MDCCXI. [1710-1711]")$from, 1710)
  expect_equal(polish_years("MDCCX.-MDCCXI. [1710-1711]")$till, 1711)
  expect_equal(polish_years("anno ut supra [=1629]")$from, 1629)
  expect_equal(polish_years("Octob. 23,1647.")$from, 1647)

  expect_equal(polish_years("M.DC.XXVIII.")$from, 1628)
  expect_equal(polish_years("M,DCC,LXXV.")$from, 1775)    
  expect_equal(polish_years("Febr. 8. 1646[7]")$from, 1647)  
  expect_equal(polish_years("Anno Dom. MDCLXXXV.")$from, 1685)  
  expect_equal(polish_years("Anno 1643. Sept. 16.")$from, 1643)

  expect_equal(polish_years("179[2?]")$from, 1792)
  expect_equal(polish_years("1757. Price 6 d.")$from, 1757)
  expect_equal(polish_years("1700. [i.e. 1700-1701]")$from, 1700)
  expect_equal(polish_years("1700. [i.e. 1700-1701]")$till, 1701)  
  expect_equal(polish_years("1698 [i.e. 1699}")$from, 1699)
  expect_equal(polish_years("169[7]]")$from, 1697)
  expect_equal(polish_years("1689 [i.e. 1689-1690]")$from, 1689)
  expect_equal(polish_years("1689 [i.e. 1689-1690]")$till, 1690)
  expect_equal(polish_years("1688 [i.e., 1689, New Style].")$from, 1689)
  expect_equal(polish_years("168[5].")$from, 1685)
  expect_equal(polish_years("1679 [i.e. 1679-1680]")$from, 1679)
  expect_equal(polish_years("1679 [i.e. 1679-1680]")$till, 1680)  
  expect_equal(polish_years("166[5?]]")$from, 1665)
  expect_equal(polish_years("166[1].")$from, 1661)
  expect_equal(polish_years("1642. Novemb, 5.")$from, 1642)
  expect_equal(polish_years("1642. June, 23.")$from, 1642)
  expect_equal(polish_years("1642 [Dec. 20]")$from, 1642)
  expect_equal(polish_years("164[2]")$from, 1642)
  expect_equal(polish_years("16[99]")$from, 1699)
  expect_equal(polish_years("16[65?]")$from, 1665)
  expect_equal(polish_years("1577. March, 7.")$from, 1577)
  expect_equal(polish_years("(1602.)")$from, 1602)
  expect_equal(polish_years("An. Dom. M.DC.XLII. [i.e. 1643-1645]")$from, 1643)
  expect_equal(polish_years("An. Dom. M.DC.XLII. [i.e. 1643-1645]")$till, 1645)  
  # expect_equal(polish_years("")$from, )  

  expect_equal(polish_years("30 March, 1646.")$from, 1646)  
  expect_equal(polish_years("26. March, 1649.")$from, 1649)  
  expect_equal(polish_years("21 of August, 1644]")$from, 1644)  
  expect_equal(polish_years("18 March, 1645 [i.e. 1646]")$from, 1646)
  expect_equal(polish_years("[25 February, 1650]")$from, 1650)  
  expect_equal(polish_years("1695/6. [1696]")$from, 1695)     
  expect_equal(polish_years("the 27. of Iuly, 1585]")$from, 1585)    
  expect_equal(polish_years("January 26. 1646. [i.e. 1647]")$from, 1647)    
  expect_equal(polish_years("1736[1735]-38.")$from, 1735)
  expect_equal(polish_years("1736[1735]-38.")$till, 1738)
  expect_equal(polish_years("Anno Domini 1552 [i.e. R. Tottell, 1565?]]")$from, 1565)    
  expect_equal(polish_years("[1639 (17 or 18 February)]]")$from, 1639)  
  expect_equal(polish_years("<1794>-")$from, 1794)
  expect_equal(polish_years("<1755?>-")$from, 1755)  
  expect_equal(polish_years("-<1888>")$till, 1888)  
  expect_equal(polish_years("Anno.1564. Mense Iulii [1564]")$from, 1564)
  expect_equal(polish_years("Septemb. 6. 1634 [i.e. 1643-1647]")$from, 1643)
  expect_equal(polish_years("Septemb. 6. 1634 [i.e. 1643-1647]")$till, 1647)

  expect_equal(polish_years("1738 [po. 1746--1752].")$from, 1746)
  expect_equal(polish_years("1738 [po. 1746--1752].")$till, 1752)

  expect_equal(polish_years("1738 [po 1746--1752].")$from, 1746)
  expect_equal(polish_years("1738 [po 1746--1752].")$till, 1752)

  expect_equal(polish_years("1738 [po. 1746]")$from, 1746)
  expect_equal(polish_years("1738 [po 1746]")$from, 1746)
  expect_equal(polish_years("prÃ¤ndÃ¤tty wuonna 1739 [po. 1771--1784]")$from, 1771)
  expect_equal(polish_years("prÃ¤ndÃ¤tty wuonna 1739 [po. 1771--1784]")$till, 1784)

  expect_equal(polish_years("1905 [oik. 15]")$from, 1915)
 
  expect_equal(polish_years("[1904-] 1905.")$from, 1904)
  expect_equal(polish_years("[1904-] 1905.")$till, 1905)  

  expect_equal(polish_years("1722?,;2:o.")$from, 1722)
  expect_equal(polish_years("1731,;4:o.")$from, 1731)
  expect_equal(polish_years("1893-1913[?]")$from, 1893)
  expect_equal(polish_years("1893-1913[?]")$till, 1913)
  expect_equal(polish_years("1913, 1915, 1916.")$from, 1913)
  expect_equal(polish_years("1913, 1915, 1916.")$till, 1916)  
  expect_equal(polish_years("a[...] 1666-1670.")$from, 1666)
  expect_equal(polish_years("a[...] 1666-1670.")$till, 1670)  
  expect_equal(polish_years("wuona jelken Christuxen syndymän 1616,;1616")$from, 1616)
  expect_equal(polish_years("étei a kh o s [=1676]")$from, 1676)
  expect_equal(polish_years("MDCCI. [1701] [1702]")$from, 1701)
  expect_equal(polish_years("MDCCI. [1701] [1702]")$till, 1702)

  expect_equal(polish_years("1889, 1-1890, 12. 1890 1890 4")$from, 1889)
  expect_equal(polish_years("1889, 1-1890, 12. 1890 1890 4")$till, 1890)

  expect_equal(polish_years("Läseåret 1885/1886-Läseåret 1889/1890. 1885 1885 2")$from, 1885)
  expect_equal(polish_years("Läseåret 1885/1886-Läseåret 1889/1890. 1885 1885 2")$till, 1890)

  expect_equal(polish_years("1890/1891-1913/1914. 1890 1890 2")$from, 1890)
  expect_equal(polish_years("1890/1891-1913/1914. 1890 1890 2")$till, 1914)

  expect_equal(polish_years("1908, 1-1909, 2. 1909 1909 2")$from, 1908)
  expect_equal(polish_years("1908, 1-1909, 2. 1909 1909 2")$till, 1909)

  expect_equal(polish_years("1891, 0-1905, 51. 1905 1905 2")$from, 1891)
  expect_equal(polish_years("1891, 0-1905, 51. 1905 1905 2")$till, 1905)


  expect_equal(polish_years("2 (1911)-1919. 1919 1919 1")$from, 1911)
  expect_equal(polish_years("2 (1911)-1919. 1919 1919 1")$till, 1919)

  expect_equal(polish_years("1900/1901-1930/1931. 1900 1900 1")$from, 1900)
  expect_equal(polish_years("1900/1901-1930/1931. 1900 1900 1")$till, 1931)

  expect_equal(polish_years("1903, häft 1-1904, häft 49. 1904 1904 1")$from, 1903)
  expect_equal(polish_years("1903, häft 1-1904, häft 49. 1904 1904 1")$till, 1904)

  expect_equal(polish_years("1901-2, (1902). 1901 1901 1")$from, 1901)
  expect_equal(polish_years("1901-2, (1902). 1901 1901 1")$till, 1902)

  expect_equal(polish_years("1852/1887-1899/1914. 1852 1852 1")$from, 1852)
  expect_equal(polish_years("1852/1887-1899/1914. 1852 1852 1")$till, 1914)

  # Could also be 1895-1900
  expect_equal(polish_years("1893, 1895-1899/1900. 1893 1893 1")$from, 1893)
  expect_equal(polish_years("1893, 1895-1899/1900. 1893 1893 1")$till, 1900)

  expect_equal(polish_years("1903-6(1908) 1903 1903 1")$from, 1903)
  expect_equal(polish_years("1903-6(1908) 1903 1903 1")$till, 1908)

  expect_equal(polish_years("9(1898)-1910. 1910 1910 1")$from, 1898)
  expect_equal(polish_years("9(1898)-1910. 1910 1910 1")$till, 1910)

  expect_equal(polish_years("1903, 1-1911, 11. 1911 1911 1")$from, 1903)
  expect_equal(polish_years("1903, 1-1911, 11. 1911 1911 1")$till, 1911)

  expect_equal(polish_years("Näytenumero 5.12.1870 ; 1871-24.1.1872. 1872 1872 1")$from, 1870)
  expect_equal(polish_years("Näytenumero 5.12.1870 ; 1871-24.1.1872. 1872 1872 1")$till, 1872)

  expect_equal(polish_years("Näytenumero 7.12.1882 ; 1883-30.12.1885. 1885 1885 1")$from, 1882)
  expect_equal(polish_years("Näytenumero 7.12.1882 ; 1883-30.12.1885. 1885 1885 1")$till, 1885)

  expect_equal(polish_years("1865, 1-1869, 4. 1869 1869 1")$from, 1865)
  expect_equal(polish_years("1865, 1-1869, 4. 1869 1869 1")$till, 1869)

  expect_equal(polish_years("1907, provn:r-1912, 1. 1912 1912 1")$from, 1907)
  expect_equal(polish_years("1907, provn:r-1912, 1. 1912 1912 1")$till, 1912)

  expect_equal(polish_years("1894, profn:r-1895, 6. 1895 1895 1")$from, 1894)
  expect_equal(polish_years("1894, profn:r-1895, 6. 1895 1895 1")$till, 1895)
  
  expect_equal(polish_years("1911, näyten:o 1-1913, 51. 1913 1913 1")$from, 1911)
  expect_equal(polish_years("1911, näyten:o 1-1913, 51. 1913 1913 1")$till, 1913)

  expect_equal(polish_years("Läseåret 1885/1886-Läseåret 1894/1895.")$from, 1885)
  expect_equal(polish_years("Läseåret 1885/1886-Läseåret 1894/1895.")$till, 1895)


  expect_equal(polish_years("Näytenumero 3.6.1882 ; 30.6.1882-19.1.1894")$from, 1882)
  expect_equal(polish_years("Näytenumero 3.6.1882 ; 30.6.1882-19.1.1894")$till, 1894)  

  expect_equal(polish_years("1(1861/1865)-8(1896/1900)")$from, 1861)
  expect_equal(polish_years("1(1861/1865)-8(1896/1900)")$till, 1900)

  expect_equal(polish_years("1(1861/1865)-8(1900)")$from, 1861)
  expect_equal(polish_years("1(1861/1865)-8(1900)")$till, 1900)

  expect_equal(polish_years("13(1861/1865)-8(1900)")$from, 1861)
  expect_equal(polish_years("13(1861/1865)-18(1900)")$till, 1900)
  
  expect_equal(polish_years("1(1865)-8(1900)")$from, 1865)
  expect_equal(polish_years("1(1865)-8(1900)")$till, 1900)

  expect_equal(polish_years("1(1865)-8 (1900)")$from, 1865)
  expect_equal(polish_years("1 (1865)-8(1900)")$till, 1900)

  expect_equal(polish_years("1884, [0] ; 1885, 1-1892, 12")$from, 1884)
  expect_equal(polish_years("1884, [0] ; 1885, 1-1892, 12")$till, 1892)

  expect_equal(polish_years("1896, [1] ; 1897, n:o 0 ; 1898, n:o 0-1898, n:o 24")$from, 1896)
  expect_equal(polish_years("1896, [1] ; 1897, n:o 0 ; 1898, n:o 0-1898, n:o 24.")$till, 1898)

  expect_equal(polish_years("1902, [1].")$from, 1902)
  expect_equal(polish_years("1903,1.")$from, 1903)

  expect_equal(polish_years("1:nen vsk. (1899/1900)")$from, 1899)
  expect_equal(polish_years("1:nen vsk. (1899/1900)")$till, 1900)

  expect_equal(polish_years("1904-1905;1904")$from, 1904)
  expect_equal(polish_years("1904-1905;1904")$till, 1905)

  expect_equal(polish_years("1921-1922;1921-1922;1922")$from, 1921)
  expect_equal(polish_years("1921-1922;1921-1922;1922")$till, 1922)

  expect_equal(polish_years("[18]91")$from, 1891)

  expect_equal(polish_years("[19]75-[19]76")$from, 1975)
  expect_equal(polish_years("[19]75-[19]76")$till, 1976)

  expect_equal(polish_years("[1901.']")$from, 1901)
  expect_equal(polish_years("[1930!]")$from, 1930)

  expect_equal(polish_years("1348")$from, 1348)
  
  expect_equal(polish_years("1781:1-13")$from, 1781)

  expect_equal(polish_years("18[29-]1830")$from, 1829)
  expect_equal(polish_years("18[29-]1830")$till, 1830)

  expect_equal(polish_years("18[30-]1833")$from, 1830)
  expect_equal(polish_years("18[30-]1833")$till, 1833)

  expect_equal(polish_years("18[35-18]42")$from, 1835)
  expect_equal(polish_years("18[35-18]42")$till, 1842)

  expect_equal(polish_years("1817: 13/12-27/12")$from, 1817)

  expect_equal(polish_years("1853,54(1,2)")$from, 1853)

  expect_equal(polish_years("1902, 29.12.-1903, 28.6.")$from, 1902)
  expect_equal(polish_years("1902, 29.12.-1903, 28.6.")$till, 1903)    

  expect_equal(polish_years("3:dje årgången, [1-26] ([1904/1905]) ; 1:sta årgången, n:o 1-54 (1905/1906)")$from, 1904)
  expect_equal(polish_years("3:dje årgången, [1-26] ([1904/1905]) ; 1:sta årgången, n:o 1-54 (1905/1906)")$till, 1906)

  expect_equal(polish_years("wuonna 1754 s. 13 päiwänä touco cuusa.")$from, 1754)
  expect_equal(polish_years("1897, 31.3.-1897, 31.3.")$from, 1897)

  expect_equal(polish_years("1896, 0 (30.9.)-1897, 31.1.")$from, 1896)
  expect_equal(polish_years("1896, 0 (30.9.)-1897, 31.1.")$till, 1897)

  expect_equal(polish_years("Pendant l'hiver 1895/1896-Pendant l'hiver 1897/1898.")$from, 1895)
  expect_equal(polish_years("Pendant l'hiver 1895/1896-Pendant l'hiver 1897/1898.")$till, 1898)

  expect_equal(polish_years("1. årg. (1880)-7. årg. (1887/1888) = 1. vsk. (1880)-7. vsk. (1887/1888)")$from, 1880)
  expect_equal(polish_years("1. årg. (1880)-7. årg. (1887/1888) = 1. vsk. (1880)-7. vsk. (1887/1888)")$till, 1888)

  expect_equal(polish_years("1. årg. (1889/1890)-27. årg. (1915/1916)")$from, 1889)
  expect_equal(polish_years("1. årg. (1889/1890)-27. årg. (1915/1916)")$till, 1916)

  expect_equal(polish_years("God' 1 (1914/1915), n:o 1-god' 3 (1916/1917), n:o 3. ")$from, 1914)
  expect_equal(polish_years("God' 1 (1914/1915), n:o 1-god' 3 (1916/1917), n:o 3. ")$till, 1917)    

  # TODO
  expect_equal(polish_years("MDCCXCV.")$from, 1795)
  expect_equal(polish_years("M.DCCXCV.")$from, 1795)
  expect_equal(polish_years("M.D.CCXCV.")$from, 1795)
  expect_equal(polish_years("M D CCXCV.")$from, 1795)
  expect_equal(polish_years("M,D,CCXCV")$from, 1795)
  expect_equal(polish_years("Printed Anno M. DC. XL.")$from, 1640)
  expect_equal(polish_years("Printed by S. Green and sold by Samuel Phillips,1̂689.")$from, 1689)
  expect_equal(polish_years("Printed for Thomas Guy,1̂689. ")$from, 1689)
  expect_equal(polish_years("M.d.xxviij [sexto calendas Julias]")$from, 1528) #check if this is xxviii or xxvii
  expect_equal(polish_years("sold [by Michael Perry],[̂1698]")$from, 1698)
  # expect_equal(polish_years("")$from, )         
  
  # expect_equal(polish_years("[1963-1864]")$from, )  
  # expect_equal(polish_years("[1963-1864]")$till, )
  # [1904/1905], [1-26] ;1905/06, 1-54. 
  #expect_equal(polish_years("100 B.C-44B.C")$from, -100)
  #expect_equal(polish_years("100B.C-44 B.C")$till, -44)
  #expect_equal(polish_years("approximately 19 B.C.-approximately 30 A.D.")$from, -19)  
  #expect_equal(polish_years("approximately 19 B.C.-approximately 30 A.D.")$till, 30)

  
})

