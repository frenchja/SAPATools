AspectNorms <- function(x) {
    # Recalculates norms for the 12 aspect scores
    # 
    # Args: x: SAPA data.frame
    # 
    # Returns: Matrix of normed aspect(s)
  
    # Test for psych package
    if (!require(psych)) {
        stop("Install psych package.")
    }
    negAssertiveness <- subset(x, select = c(q_1730, q_1848, 
        q_1992, q_2161, q_1151, q_1242, q_1913))
    posAssertiveness <- subset(x, select = c(q_131, q_158, q_279, 
        q_698, q_832, q_861, q_1104, q_1110, q_1393, q_1810, 
        q_260, q_455, q_1055, q_1205, q_1652, q_1768))
    negAssertivenessMeans <- round((7 - describe(negAssertiveness)[, 
        3]), 2)
    names(negAssertivenessMeans) <- colnames(negAssertiveness)
    posAssertivenessMeans <- round(describe(posAssertiveness)[, 
        3], 2)
    names(posAssertivenessMeans) <- colnames(posAssertiveness)
    AssertivenessItemMeans <- c(negAssertivenessMeans, posAssertivenessMeans)
    negAssertivenessSDs <- round(describe(negAssertiveness)[, 
        4], 2)
    names(negAssertivenessSDs) <- colnames(negAssertiveness)
    posAssertivenessSDs <- round(describe(posAssertiveness)[, 
        4], 2)
    names(posAssertivenessSDs) <- colnames(posAssertiveness)
    AssertivenessItemSDs <- c(negAssertivenessSDs, posAssertivenessSDs)
    negBalance <- subset(x, select = c(q_974, q_1479, q_108, 
        q_549, q_811, q_960, q_979, q_986, q_994, q_1020, q_1099, 
        q_1495, q_1505, q_1775, q_2786, q_952, q_995, q_52, q_413, 
        q_497, q_965, q_66, q_105, q_126, q_332, q_890, q_1989))
    posBalance <- subset(x, select = c(q_1683, q_177, q_248, 
        q_727, q_1596, q_1610, q_1616, q_2765, q_1585, q_176, 
        q_1185, q_1588, q_820, q_1578, q_1677))
    negBalanceMeans <- round((7 - describe(negBalance)[, 3]), 
        2)
    names(negBalanceMeans) <- colnames(negBalance)
    posBalanceMeans <- round(describe(posBalance)[, 3], 2)
    names(posBalanceMeans) <- colnames(posBalance)
    BalanceItemMeans <- c(negBalanceMeans, posBalanceMeans)
    negBalanceSDs <- round(describe(negBalance)[, 4], 2)
    names(negBalanceSDs) <- colnames(negBalance)
    posBalanceSDs <- round(describe(posBalance)[, 4], 2)
    names(posBalanceSDs) <- colnames(posBalance)
    BalanceItemSDs <- c(negBalanceSDs, posBalanceSDs)
    negBoldness <- subset(x, select = c(q_53, q_98, q_253, q_334, 
        q_463, q_539, q_1159, q_1260, q_1446, q_1447, q_1448, 
        q_1490, q_1668, q_1706, q_1759, q_1853, q_1924, q_2001, 
        q_2018))
    posBoldness <- subset(x, select = c(q_1306, q_292, q_179, 
        q_251, q_721, q_804, q_1367, q_1573, q_1679, q_1681, 
        q_1993, q_184))
    negBoldnessMeans <- round((7 - describe(negBoldness)[, 3]), 
        2)
    names(negBoldnessMeans) <- colnames(negBoldness)
    posBoldnessMeans <- round(describe(posBoldness)[, 3], 2)
    names(posBoldnessMeans) <- colnames(posBoldness)
    BoldnessItemMeans <- c(negBoldnessMeans, posBoldnessMeans)
    negBoldnessSDs <- round(describe(negBoldness)[, 4], 2)
    names(negBoldnessSDs) <- colnames(negBoldness)
    posBoldnessSDs <- round(describe(posBoldness)[, 4], 2)
    names(posBoldnessSDs) <- colnames(posBoldness)
    BoldnessItemSDs <- c(negBoldnessSDs, posBoldnessSDs)
    negCompassion <- subset(x, select = c(q_200, q_838, q_146, 
        q_195, q_460, q_668, q_1774))
    posCompassion <- subset(x, select = c(q_150, q_159, q_1041, 
        q_1053, q_1206, q_1364, q_1385, q_1419, q_1705, q_1792, 
        q_1832, q_844, q_1162, q_1307, q_1763, q_1766, q_1031, 
        q_1624))
    negCompassionMeans <- round((7 - describe(negCompassion)[, 
        3]), 2)
    names(negCompassionMeans) <- colnames(negCompassion)
    posCompassionMeans <- round(describe(posCompassion)[, 3], 
        2)
    names(posCompassionMeans) <- colnames(posCompassion)
    CompassionItemMeans <- c(negCompassionMeans, posCompassionMeans)
    negCompassionSDs <- round(describe(negCompassion)[, 4], 2)
    names(negCompassionSDs) <- colnames(negCompassion)
    posCompassionSDs <- round(describe(posCompassion)[, 4], 2)
    names(posCompassionSDs) <- colnames(posCompassion)
    CompassionItemSDs <- c(negCompassionSDs, posCompassionSDs)
    negSociability <- subset(x, select = c(q_55, q_241, q_403, 
        q_690, q_712, q_901, q_1027, q_1114, q_1180, q_1480, 
        q_1575, q_1643, q_1671, q_1685, q_2024, q_2900, q_140, 
        q_2160, q_1196, q_1583, q_1635))
    posSociability <- subset(x, select = c(q_254, q_262, q_744, 
        q_815, q_819, q_1064, q_1379, q_1709, q_1742, q_1795, 
        q_1803, q_1899, q_1904, q_1043, q_1243, q_1410, q_1703, 
        q_1943))
    negSociabilityMeans <- round((7 - describe(negSociability)[, 
        3]), 2)
    names(negSociabilityMeans) <- colnames(negSociability)
    posSociabilityMeans <- round(describe(posSociability)[, 3], 
        2)
    names(posSociabilityMeans) <- colnames(posSociability)
    SociabilityItemMeans <- c(negSociabilityMeans, posSociabilityMeans)
    negSociabilitySDs <- round(describe(negSociability)[, 4], 
        2)
    names(negSociabilitySDs) <- colnames(negSociability)
    posSociabilitySDs <- round(describe(posSociability)[, 4], 
        2)
    names(posSociabilitySDs) <- colnames(posSociability)
    SociabilityItemSDs <- c(negSociabilitySDs, posSociabilitySDs)
    negHonesty <- subset(x, select = c(q_40, q_500, q_501, q_1543, 
        q_1747, q_1780, q_1762, q_2029))
    posHonesty <- subset(x, select = c(q_185, q_1633, q_1752, 
        q_1867, q_2003, q_2016, q_2021, q_2853))
    negHonestyMeans <- round((7 - describe(negHonesty)[, 3]), 
        2)
    names(negHonestyMeans) <- colnames(negHonesty)
    posHonestyMeans <- round(describe(posHonesty)[, 3], 2)
    names(posHonestyMeans) <- colnames(posHonesty)
    HonestyItemMeans <- c(negHonestyMeans, posHonestyMeans)
    negHonestySDs <- round(describe(negHonesty)[, 4], 2)
    names(negHonestySDs) <- colnames(negHonesty)
    posHonestySDs <- round(describe(posHonesty)[, 4], 2)
    names(posHonestySDs) <- colnames(posHonesty)
    HonestyItemSDs <- c(negHonestySDs, posHonestySDs)
    negHumility <- subset(x, select = c(q_225, q_364, q_23, q_1517, 
        q_1969, q_154, q_156, q_157, q_401, q_1054, q_1296, q_1372, 
        q_1536, q_1555, q_1667, q_1808, q_1871, q_1894, q_1896, 
        q_2008))
    posHumility <- subset(x, select = c(q_2025, q_152, q_525, 
        q_704, q_711, q_716, q_917, q_1276, q_1653))
    negHumilityMeans <- round((7 - describe(negHumility)[, 3]), 
        2)
    names(negHumilityMeans) <- colnames(negHumility)
    posHumilityMeans <- round(describe(posHumility)[, 3], 2)
    names(posHumilityMeans) <- colnames(posHumility)
    HumilityItemMeans <- c(negHumilityMeans, posHumilityMeans)
    negHumilitySDs <- round(describe(negHumility)[, 4], 2)
    names(negHumilitySDs) <- colnames(negHumility)
    posHumilitySDs <- round(describe(posHumility)[, 4], 2)
    names(posHumilitySDs) <- colnames(posHumility)
    HumilityItemSDs <- c(negHumilitySDs, posHumilitySDs)
    negIndustry <- subset(x, select = c(q_22, q_602, q_626, q_636, 
        q_637, q_665, q_673, q_1024, q_1173, q_1254, q_1395, 
        q_1397, q_1401, q_1424, q_1452, q_1483, q_1537, q_1563, 
        q_1696, q_1754, q_107, q_706, q_904, q_1429, q_1521, 
        q_1949, q_1255))
    posIndustry <- subset(x, select = c(q_76, q_519, q_520, q_962, 
        q_985, q_1422, q_1550, q_1979, q_2737, q_44, q_491, q_930, 
        q_988))
    negIndustryMeans <- round((7 - describe(negIndustry)[, 3]), 
        2)
    names(negIndustryMeans) <- colnames(negIndustry)
    posIndustryMeans <- round(describe(posIndustry)[, 3], 2)
    names(posIndustryMeans) <- colnames(posIndustry)
    IndustryItemMeans <- c(negIndustryMeans, posIndustryMeans)
    negIndustrySDs <- round(describe(negIndustry)[, 4], 2)
    names(negIndustrySDs) <- colnames(negIndustry)
    posIndustrySDs <- round(describe(posIndustry)[, 4], 2)
    names(posIndustrySDs) <- colnames(posIndustry)
    IndustryItemSDs <- c(negIndustrySDs, posIndustrySDs)
    negIntellect <- subset(x, select = c(q_1146, q_316, q_1088, 
        q_1253))
    posIntellect <- subset(x, select = c(q_79, q_128, q_132, 
        q_151, q_492, q_493, q_516, q_761, q_923, q_1090, q_1388, 
        q_1392, q_2756, q_240, q_422, q_946, q_1050, q_1327, 
        q_1834))
    negIntellectMeans <- round((7 - describe(negIntellect)[, 
        3]), 2)
    names(negIntellectMeans) <- colnames(negIntellect)
    posIntellectMeans <- round(describe(posIntellect)[, 3], 2)
    names(posIntellectMeans) <- colnames(posIntellect)
    IntellectItemMeans <- c(negIntellectMeans, posIntellectMeans)
    negIntellectSDs <- round(describe(negIntellect)[, 4], 2)
    names(negIntellectSDs) <- colnames(negIntellect)
    posIntellectSDs <- round(describe(posIntellect)[, 4], 2)
    names(posIntellectSDs) <- colnames(posIntellect)
    IntellectItemSDs <- c(negIntellectSDs, posIntellectSDs)
    negOpenness <- subset(x, select = c(q_194, q_608, q_609, 
        q_610, q_611, q_651, q_670, q_747, q_1083, q_1300, q_1301, 
        q_1676, q_1861, q_1964, q_2005, q_2775, q_2891, q_322, 
        q_612, q_1675, q_1682, q_1687))
    posOpenness <- subset(x, select = c(q_94, q_634, q_757, q_1058, 
        q_1132, q_1232, q_1609, q_1738, q_1761, q_1893, q_2011, 
        q_348, q_776, q_964, q_1389, q_1441, q_1648))
    negOpennessMeans <- round((7 - describe(negOpenness)[, 3]), 
        2)
    names(negOpennessMeans) <- colnames(negOpenness)
    posOpennessMeans <- round(describe(posOpenness)[, 3], 2)
    names(posOpennessMeans) <- colnames(posOpenness)
    OpennessItemMeans <- c(negOpennessMeans, posOpennessMeans)
    negOpennessSDs <- round(describe(negOpenness)[, 4], 2)
    names(negOpennessSDs) <- colnames(negOpenness)
    posOpennessSDs <- round(describe(posOpenness)[, 4], 2)
    names(posOpennessSDs) <- colnames(posOpenness)
    OpennessItemSDs <- c(negOpennessSDs, posOpennessSDs)
    negOrderliness <- subset(x, select = c(q_1511, q_169, q_170, 
        q_582))
    posOrderliness <- subset(x, select = c(q_571, q_124, q_321, 
        q_530, q_554, q_559, q_619, q_1063, q_1321, q_1333, q_1374, 
        q_1507, q_1916, q_931, q_1201, q_1290, q_1657, q_1915, 
        q_1917))
    negOrderlinessMeans <- round((7 - describe(negOrderliness)[, 
        3]), 2)
    names(negOrderlinessMeans) <- colnames(negOrderliness)
    posOrderlinessMeans <- round(describe(posOrderliness)[, 3], 
        2)
    names(posOrderlinessMeans) <- colnames(posOrderliness)
    OrderlinessItemMeans <- c(negOrderlinessMeans, posOrderlinessMeans)
    negOrderlinessSDs <- round(describe(negOrderliness)[, 4], 
        2)
    names(negOrderlinessSDs) <- colnames(negOrderliness)
    posOrderlinessSDs <- round(describe(posOrderliness)[, 4], 
        2)
    names(posOrderlinessSDs) <- colnames(posOrderliness)
    OrderlinessItemSDs <- c(negOrderlinessSDs, posOrderlinessSDs)
    negPoliteness <- subset(x, select = c(q_80, q_101, q_139, 
        q_141, q_142, q_238, q_239, q_331, q_477, q_538, q_594, 
        q_871, q_898, q_915, q_958, q_966, q_997, q_1033, q_1051, 
        q_1150, q_1357, q_1601, q_1725, q_1957, q_1163, q_1359, 
        q_1663, q_1765))
    posPoliteness <- subset(x, select = c(q_6, q_39, q_134, q_145, 
        q_217, q_278, q_926, q_1373, q_1571, q_1577, q_1591, 
        q_1787, q_1869, q_319, q_1556))
    negPolitenessMeans <- round((7 - describe(negPoliteness)[, 
        3]), 2)
    names(negPolitenessMeans) <- colnames(negPoliteness)
    posPolitenessMeans <- round(describe(posPoliteness)[, 3], 
        2)
    names(posPolitenessMeans) <- colnames(posPoliteness)
    PolitenessItemMeans <- c(negPolitenessMeans, posPolitenessMeans)
    negPolitenessSDs <- round(describe(negPoliteness)[, 4], 2)
    names(negPolitenessSDs) <- colnames(negPoliteness)
    posPolitenessSDs <- round(describe(posPoliteness)[, 4], 2)
    names(posPolitenessSDs) <- colnames(posPoliteness)
    PolitenessItemSDs <- c(negPolitenessSDs, posPolitenessSDs)
    Means <- c(CompassionItemMeans, PolitenessItemMeans, IndustryItemMeans, 
        OrderlinessItemMeans, BalanceItemMeans, BoldnessItemMeans, 
        AssertivenessItemMeans, SociabilityItemMeans, HonestyItemMeans, 
        HumilityItemMeans, IntellectItemMeans, OpennessItemMeans)
    SDs <- c(CompassionItemSDs, PolitenessItemSDs, IndustryItemSDs, 
        OrderlinessItemSDs, BalanceItemSDs, BoldnessItemSDs, 
        AssertivenessItemSDs, SociabilityItemSDs, HonestyItemSDs, 
        HumilityItemSDs, IntellectItemSDs, OpennessItemSDs)
    aspectMatrix <- rbind(Means, SDs)
    return(aspectMatrix)
}
