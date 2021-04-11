{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE FlexibleInstances #-}
module DataKinds where

data Auth = Regular | Admin

-- data User a where 
data User :: Auth -> * where 
  RegularUser :: User 'Regular
  AdminUser :: User 'Admin

onlyAdmin :: User 'Admin -> IO ()
onlyAdmin _ = putStrLn "yep, you're an admin"

test = do
  onlyAdmin AdminUser
    -- won't compile
--   onlyAdmin RegularUser

-------------------

-- data JobDescription = JobOne { n :: Int }
--                     | JobTwo
--                     | JobThree { n :: Int }
--                     deriving (Show, Eq)

-- taskOneWorker :: JobDescription -> IO ()
-- taskOneWorker t = putStrLn $ "n: " <> show (n t)

-- go :: IO ()
-- go = do
--     taskOneWorker (JobOne 1)
--     -- taskOneWorker JobTwo
--     taskOneWorker (JobThree 3)

--------------------

-- data JobDescription = JobOne
--                     | JobTwo
--                     | JobThree
--   deriving (Show, Eq)

-- data SJobDescription :: JobDescription -> * where
--     -- SJobOne :: { jobOneN :: Int } -> SJobDescription 'JobOne
--     SJobOne :: Int -> SJobDescription 'JobOne
--     SJobTwo :: SJobDescription 'JobTwo
--     SJobThree :: Int -> SJobDescription 'JobThree

-- taskOneWorker :: SJobDescription 'JobOne -> IO ()
-- -- taskOneWorker t = do
--     -- putStrLn $ "Job: " <> show (jobOneN t)
-- taskOneWorker (SJobOne n) = do
--     putStrLn $ "Job: " <> show n

-- go :: IO ()
-- go = do
--   -- this typechecks: 
--   taskOneWorker (SJobOne 10)

--   -- these two don't type-check:
--   taskOneWorker SJobTwo
--   taskOneWorker (SJobThree 10)