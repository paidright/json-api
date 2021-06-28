{-# LANGUAGE OverloadedStrings #-}

module Network.JSONApi.Pagination (
    Pagination (..)
  , PageIndex (..)
  , PageSize (..)
  , ResourceCount (..)
  , Strategy (..)
  , mkPaginationLinks
) where

import Control.Monad (guard)
import Data.Aeson (ToJSON (toJSON), (.=), object)
import Network.JSONApi.Link (Links, Rel, mkLinks)
import Network.JSONApi.Meta (MetaObject (typeName))
import Network.URL (URL, add_param)
import qualified GHC.Generics as G
import Control.DeepSeq (NFData)

{- |
Wrapper type for the various components of pagination being page size, page index
and the number of resources in total.
-}
data Pagination = Pagination {
                      getPaginationPageIndex :: PageIndex
                    , getPaginationPageSize :: PageSize
                    , getPaginationResourceCount :: ResourceCount
                  } deriving G.Generic

instance NFData Pagination


instance ToJSON Pagination where
  toJSON (Pagination (PageIndex num) (PageSize size)  (ResourceCount count)) =
    object [
        "pageSize" .= size
      , "currentPage" .= num
      , "totalDocuments" .= count
      ]

{- |
Pagination can be used as a meta object if required in addition to the links generated
for paging.
-}
instance MetaObject Pagination where
  typeName _ = "pagination"

{- |
We can specify limits on the number of rows we would like back from the database
-}
newtype PageSize = PageSize {
  getPageSize :: Int
} deriving (Show, NFData)

newtype PageIndex = PageIndex {
  getPageIndex :: Int
} deriving (Show, NFData)

newtype ResourceCount = ResourceCount {
  getResourceCount :: Int
} deriving (Show, NFData)

{- |
Pagination strategies are commonly implemented by the server of which Page and Offset
are commonly used.
-}
data Strategy = PageStrategy | OffsetStrategy

{- |
Build relative links for a collection of resources of type ResourceEntity.

The first page for 'PageStrategy' is \'@'PageIndex' 1@\'.

The first page for 'OffsetStrategy' is \'@'PageIndex' 0@\'.

Zero resources means zero pages, so no links will be generated for 
@'ResourceCount' 0@.
-}
mkPaginationLinks :: Strategy -> URL -> Pagination -> Links
mkPaginationLinks strategy baseUrl page =
  mkLinks $
  foldMap (pageLink "first") (firstPageIndex strategy page) <>
  foldMap (pageLink "last") (lastPageIndex strategy page) <>
  foldMap (pageLink "next") (nextPageIndex strategy page) <>
  foldMap (pageLink "prev") (prevPageIndex strategy page)
    where
      pgSize = getPageSize $ getPaginationPageSize page
      pageLink name (PageIndex index) = [ mkPaginationLink strategy name baseUrl index pgSize ]

nextPageIndex :: Strategy -> Pagination -> Maybe PageIndex
nextPageIndex PageStrategy pagination = do
  let 
    count = pageCount pagination
    PageIndex pageIndex = getPaginationPageIndex pagination
  PageIndex (pageIndex + 1) <$ guard (pageIndex < count)
nextPageIndex OffsetStrategy pagination = do
  let
    count = pageCount pagination
    PageIndex pageIndex = getPaginationPageIndex pagination
  PageIndex (pageIndex + 1) <$ guard (pageIndex < count - 1)

prevPageIndex :: Strategy -> Pagination -> Maybe PageIndex
prevPageIndex strategy pagination = do
  PageIndex firstIndex <- firstPageIndex strategy pagination
  let PageIndex pageIndex = getPaginationPageIndex pagination
  PageIndex (pageIndex - 1) <$ guard (pageIndex > firstIndex)

-- | Calculate the total number of pages using page size and total resource count.
pageCount :: Pagination -> Int
pageCount (Pagination _ pageSize resourceCount) =
  if r == 0 then q else q + 1
  where
    (q, r) = quotRem (getResourceCount resourceCount) (getPageSize pageSize)

{- |
Helper function used to generate a single pagination link.
-}
mkPaginationLink :: Strategy -> Rel -> URL -> Int -> Int -> (Rel, URL)
mkPaginationLink strategy key baseUrl pageNo pageSize =
  (key, link)
    where
      pageNoUrl = add_param baseUrl (strategyToQueryStringNumberKey strategy, show pageNo)
      link      = add_param pageNoUrl (strategyToQueryStringSizeKey strategy, show pageSize)

{- |
@'PageStragegy'@ pages start at 1.

@'OffsetStrategy'@ pages start at 0.
-}
firstPageIndex :: Strategy -> Pagination -> Maybe PageIndex
firstPageIndex PageStrategy p = PageIndex 1 <$ guard (pageCount p > 0)
firstPageIndex OffsetStrategy p = PageIndex 0 <$ guard (pageCount p > 0)

lastPageIndex :: Strategy -> Pagination -> Maybe PageIndex
lastPageIndex PageStrategy page = do
  let count = pageCount page
  PageIndex count <$ guard (count > 0) 
lastPageIndex OffsetStrategy page = do
  let count = pageCount page
  PageIndex (count - 1) <$ guard (count > 0) 

{- |
Simple pattern matcher than translates a Strategy to a query string element name.
-}
strategyToQueryStringNumberKey :: Strategy -> String
strategyToQueryStringNumberKey PageStrategy = "page[number]"
strategyToQueryStringNumberKey OffsetStrategy = "page[offset]"

strategyToQueryStringSizeKey :: Strategy -> String
strategyToQueryStringSizeKey PageStrategy = "page[size]"
strategyToQueryStringSizeKey OffsetStrategy = "page[limit]"
