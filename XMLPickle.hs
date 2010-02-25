module XMLPickle where


import Text.XML.HXT.Arrow
import PPTStream

-- data types -------------------------------------------------------------------------
-- end of data types ------------------------------------------------------------------
-- instances --------------------------------------------------------------------------
instance XmlPickler PPTNode where
  xpickle = xpPPTNode

instance XmlPickler PPTRecordHeader where
  xpickle = xpWrap ( uncurry4 PPTRecordHeader
                   , \ (PPTRecordHeader ver inst recType len) -> (ver, inst, recType, len)) $
            xpElem "header" $
            xp4Tuple (xpAttr "recVer" xpPrim) (xpAttr "recInstance" xpPrim)
                     (xpAttr "recType" xpPrim) (xpAttr "recLen" xpPrim)

xpPPTNode = xpAlt tag ps
  where tag (PPTAtom _ _) = 0
        tag (PPTContainer _ _) = 1
        ps = [ xpWrap (  uncurry PPTAtom
                       , \ (PPTAtom header bin) -> (header, bin)) (xpElem "atom" $
                                                                   xpPair xpickle xpPrim) -- xpPrim for ByteString
                      ,
               xpWrap (  uncurry PPTContainer
                       , \ (PPTContainer header children) -> (header, children)) (xpElem "container" $
                                                                                  xpPair xpickle (xpList xpickle))
             ]
-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------

