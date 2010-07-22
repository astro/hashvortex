module Versions where

import Data.Binary
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy as W8
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString as SW8
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Binary.Get

import BEncoding

data Version = Missing
             | Version !ClientType
               deriving (Eq, Ord)
instance Show Version where
    show Missing = "Missing"
    show (Version clientType)
        = show clientType

data ClientType = Unknown
                | Ares
                | Arctic
                | Artemis
                | BitPump
                | Azureus
                | BitBuddy
                | BitComet
                | Bitflu
                | BTG
                | BitBlinder
                | BitTorrentPro
                | BitRocket
                | BTSlave
                | BitWombat
                | BittorrentX
                | EnhancedCTorrent
                | CTorrent
                | DelugeTorrent
                | PropagateDataClient
                | EBit
                | ElectricSheep
                | FileCroc
                | FoxTorrent
                | GSTorrent
                | Hekate
                | Halite
                | Hydranode
                | KGet
                | KTorrent
                | LeechCraft
                | LHABC
                | Lphant
                | LibTorrent
                | LimeWire
                | MonoTorrent
                | MooPolice
                | Miro
                | MoonlightTorrent
                | NetTransport
                | OneSwarm
                | OmegaTorrent
                | Pando
                | QBittorrent
                | QQDownload
                | Qt4TorrentExample
                | Retriever
                | RezTorrent
                | ShareazaAlphaBeta
                | Swiftbit
                | Thunder
                | SoMud
                | SwarmScope
                | SymTorrent
                | Sharktorrent
                | Shareaza
                | TorrentDotNET
                | Transmission
                | Torrentstorm
                | TuoTu
                | ULeecher
                | UTorrentMac
                | UTorrent
                | Vagaa
                | BitLet
                | FireTorrent
                | Xunlei
                | XSwifter
                | XanTorrent
                | Xtorrent
                | ZipTorrent 
                  deriving (Show, Eq, Ord)

clientIdMap
    = Map.fromList $
      map (\(id, client) ->
              (SB8.pack id, client)
          ) [("AG", Ares),
             ("A~", Ares),
             ("AR", Arctic),
             ("AT", Artemis),
             ("AX", BitPump),
             ("AZ", Azureus),
             ("BB", BitBuddy),
             ("BC", BitComet),
             ("BF", Bitflu),
             ("BG", BTG),
             ("BL", BitBlinder),
             ("BP", BitTorrentPro),
             ("BR", BitRocket),
             ("BS", BTSlave),
             ("BW", BitWombat),
             ("BX", BittorrentX),
             ("CD", EnhancedCTorrent),
             ("CT", CTorrent),
             ("DE", DelugeTorrent),
             ("DP", PropagateDataClient),
             ("EB", EBit),
             ("ES", ElectricSheep),
             ("FC", FileCroc),
             ("FT", FoxTorrent),
             ("GS", GSTorrent),
             ("HK", Hekate),
             ("HL", Halite),
             ("HN", Hydranode),
             ("KG", KGet),
             ("KT", KTorrent),
             ("LC", LeechCraft),
             ("LH", LHABC),
             ("LP", Lphant),
             ("LT", LibTorrent),
             ("lt", LibTorrent),
             ("LW", LimeWire),
             ("MO", MonoTorrent),
             ("MP", MooPolice),
             ("MR", Miro),
             ("MT", MoonlightTorrent),
             ("NX", NetTransport),
             ("OS", OneSwarm),
             ("OT", OmegaTorrent),
             ("PD", Pando),
             ("qB", QBittorrent),
             ("QD", QQDownload),
             ("QT", Qt4TorrentExample),
             ("RT", Retriever),
             ("RZ", RezTorrent),
             ("S~", ShareazaAlphaBeta),
             ("SB", Swiftbit),
             ("SD", Thunder),
             ("SM", SoMud),
             ("SS", SwarmScope),
             ("ST", SymTorrent),
             ("st", Sharktorrent),
             ("SZ", Shareaza),
             ("TN", TorrentDotNET),
             ("TR", Transmission),
             ("TS", Torrentstorm),
             ("TT", TuoTu),
             ("UL", ULeecher),
             ("UM", UTorrentMac),
             ("UT", UTorrent),
             ("VG", Vagaa),
             ("WT", BitLet),
             ("WY", FireTorrent),
             ("XL", Xunlei),
             ("XS", XSwifter),
             ("XT", XanTorrent),
             ("XX", Xtorrent),
             ("ZT", ZipTorrent)]

clientIdVersion clientId
    = case Map.lookup clientId clientIdMap of
        Just version -> version
        Nothing -> Unknown

parseKRPCVersion pkt
    = case pkt `bdictLookup` "v" of
        Just (BString v)
            | B8.length v >= 2 ->
                runGet decoder v
        Just _ -> Version Unknown
        _ -> Missing

decoder :: Get Version
decoder = Version <$>
          clientIdVersion <$>
          getBytes 2
