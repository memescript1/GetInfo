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
    System = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628,MuWings = 1623, MuBlade = 1534, Whanderson = 1678, HiperMU = 455}),
    WarInCastle = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534, Whanderson = 1678}),
    KingOfPVP = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534, Whanderson = 1678}),
    LastMan = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    PartySurvival = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534,}),
    QuestDaily = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    QuestSystem = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, MuWings = 1623, MuBlade = 1534,Whanderson = 1678, HiperMU = 455}),
    GuildVSGuild = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, Whanderson = 1678}),
    ZenControl = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    OpenMap = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    MonsterCollector = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    MonsterEvolution = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    NewRace = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    MixCommand = createProtectedTable({LoaD = 683,MuWings = 1623, MuBlade = 1534}),
    Auction = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    CaptureTheFlag = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    CastleSiegeReward = createProtectedTable({LoaD = 683, LoaD2 = 1644,Whanderson = 1678}),
    CustomCMUP = createProtectedTable({LoaD = 683, LoaD2 = 1644,}),
    CustomJewelWings = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    InvasionManager = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534,Whanderson = 1678}),
    InvasionSimple = createProtectedTable({LoaD = 683,MuWings = 1623, MuBlade = 1534}),
    IceTemple = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, Whanderson = 1678}),
    GensWar = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534}),
    DCFriend = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534}),
    HideAndSeek = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, Whanderson = 1678}),
    JewelBank = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534}),
    CustomAddWings4 = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    JewelWorkshop = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    GameOfThrones = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    MasterResetReward = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    MoveHack = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534,Whanderson = 1678}),
    MResetExtraController = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    NpcSpeak = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534}),
    Quiz = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534}),
    PKFree = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, MuWings = 1623, MuBlade = 1534}),
    Raffle = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, MuWings = 1623, MuBlade = 1534}),
    ResetExtraControler = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    SimonSays = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, }),
    Tag = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    ToG = createProtectedTable({LoaD = 683, LoaD2 = 1644,Gabriel = 1508, Dejota = 1628, }),
    Wing4Roulette = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, Whanderson = 1678}),
    KillFeed = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, MuWings = 1623, MuBlade = 1534,Whanderson = 1678}),
    SpeedServer = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, MuWings = 1623, MuBlade = 1534 }),
    ItemChange = createProtectedTable({LoaD = 683, LoaD2 = 1644,Dejota = 1628, MuWings = 1623, MuBlade = 1534 }),

})

L_StartSpecial = createProtectedTable({
    LoaD = 683, LoaD2 = 1644,
})

