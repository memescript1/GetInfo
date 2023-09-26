local function createProtectedTable(tbl)
    local protectedTable = tbl

    local meta = {
        __index = function(_, key)
            return protectedTable[key]
        end,
        __newindex = function(_, key, _)
            error("Attempt to modify protected table", 2)
        end
    }

    return setmetatable({}, meta)
end

L_Start = createProtectedTable({
    System = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Whanderson = 1678,
        HiperMU = 455,
        Bruno = 1626,
        Draco = 1649,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
        Gleyson1 = 1123,
        Gleyson2 = 1639,
        Ponce = 1653,
        ponneyhd = 1293,

    }),
    WarInCastle = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Whanderson = 1678,
        Gleyson1 = 1123,
        Gleyson2 = 1639,
    }),
    KingOfPVP = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Whanderson = 1678,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    LastMan = createProtectedTable({
        Dejota = 1628,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    PartySurvival = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Gleyson1 = 1123,
        Gleyson2 = 1639,
        Ponce = 1653,
    }),
    QuestDaily = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        Draco = 1649,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    QuestSystem = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Whanderson = 1678,
        HiperMU = 455
    }),
    GuildVSGuild = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        Whanderson = 1678,
        Draco = 1649,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    ZenControl = createProtectedTable({
        Dejota = 1628,
        Bruno = 1626,
        Draco = 1649,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    OpenMap = createProtectedTable({
        Dejota = 1628,
        Draco = 1649,
    }),
    MonsterCollector = createProtectedTable({
        Dejota = 1628,
        Gleyson1 = 1123,
        Gleyson2 = 1639,
    }),
    MonsterEvolution = createProtectedTable({
        Dejota = 1628,
        Gleyson1 = 1123,
        Gleyson2 = 1639,
        Ponce = 1653,
    }),
    NewRace = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    MixCommand = createProtectedTable({
        MuWings = 1623,
        MuBlade = 1534,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    Auction = createProtectedTable({
        Dejota = 1628,
        Draco = 1649,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
        Gleyson1 = 1123,
        Gleyson2 = 1639,
    }),
    CaptureTheFlag = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    CastleSiegeReward = createProtectedTable({
        Whanderson = 1678
    }),
    CustomCMUP = createProtectedTable({
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    CustomJewelWings = createProtectedTable({
        Dejota = 1628,
    }),
    InvasionManager = createProtectedTable({
        LoaD2 = 1644,
        Gabriel = 1508,
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Whanderson = 1678,
        Bruno = 1626,
        Draco = 1649,
    }),
    InvasionSimple = createProtectedTable({
        MuWings = 1623,
        MuBlade = 1534,
        ponneyhd = 1293,
    }),
    IceTemple = createProtectedTable({
        Dejota = 1628,
        Whanderson = 1678,
        Gleyson1 = 1123,
        Gleyson2 = 1639,
    }),
    GensWar = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534
    }),
    DCFriend = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Bruno = 1626,
        Whanderson = 1678,
    }),
    HideAndSeek = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        Love1370 = 1370,
        Love1384 = 1384,
        Love1388 = 1388,
        Love1353 = 1353,
        Love1385 = 1385,
        Love1386 = 1386,
        Love1387 = 1387,
        Love1372 = 1372,
    }),
    JewelBank = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534
    }),
    CustomAddWings4 = createProtectedTable({
        Dejota = 1628,
        Bruno = 1626,
    }),
    JewelWorkshop = createProtectedTable({
        Dejota = 1628,
    }),
    JewelConverter = createProtectedTable({
        Dejota = 1628,
    }),
    GameOfThrones = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
    }),
    MasterResetReward = createProtectedTable({
        Dejota = 1628,
        Bruno = 1626,
    }),
    MoveHack = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Whanderson = 1678
    }),
    MResetExtraController = createProtectedTable({
        Dejota = 1628,
        Bruno = 1626,
    }),
    NpcSpeak = createProtectedTable({
        MuWings = 1623,
        MuBlade = 1534
    }),
    Quiz = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Draco = 1649,
    }),
    PKFree = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Draco = 1649,
    }),
    Raffle = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534
    }),
    ResetExtraControler = createProtectedTable({
        Dejota = 1628,
        Bruno = 1626,
    }),
    SimonSays = createProtectedTable({
        Dejota = 1628,
        Bruno = 1626,
        ponneyhd = 1293,
    }),
    Tag = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
    }),
    ToG = createProtectedTable({
        Gabriel = 1508,
        Dejota = 1628,
        Draco = 1649,
    }),
    Wing4Roulette = createProtectedTable({
        Dejota = 1628,
        Whanderson = 1678
    }),
    KillFeed = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
        Whanderson = 1678,
        ponneyhd = 1293,
    }),
    SpeedServer = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
    }),
    ItemChange = createProtectedTable({
        Dejota = 1628,
        MuWings = 1623,
        MuBlade = 1534,
    }),
    ZombieEvent = createProtectedTable({
        Dejota = 1628,
    }),
    BossController = createProtectedTable({
        Dejota = 1628,
    }),
    FarmBlock = createProtectedTable({
        Dejota = 1628,
    }),
    OnlineDraw = createProtectedTable({
        ponneyhd = 1293,
    }),

})

L_StartSpecial = createProtectedTable({
    LoaD = createProtectedTable({841,os.time({year=2030, month=9, day=9, hour=23, min=00, sec=0})}),
})