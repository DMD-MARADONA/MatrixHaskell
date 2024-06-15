module Main where
    import LibMatrix
    import System.IO
    main :: IO ()
    main = do
        putStrLn "Matrix Calculator"
        putStrLn "1. Identity Matrix"
        putStrLn "2. Matrix Transpose"
        putStrLn "3. Matrix Scalar Multiplication"
        putStrLn "4. Matrix Addition"
        putStrLn "5. Matrix Subtraction"
        putStrLn "6. Matrix Multiplication"
        putStrLn "Select an Action by Number"
        hFlush stdout
        _action <- getLine
        case _action of
            "1" -> carryActionIdentity
            "2" -> carryActionTranspose
            "3" -> carryActionScalar
            "4" -> carryAction addMatrix "Adittion"
            "5" -> carryAction subMatrix "Subtraction"
            "6" -> carryAction mulMatrix "Multiplication"
            _   -> putStrLn "Wrong Selection"

    inputMatrix :: String -> IO Matrix
    inputMatrix name = do
        putStrLn $ "No. Rows " ++ name ++ ": "
        norows <- readLn
        putStrLn $ "No. Cols " ++ name ++ ": "
        nocols <- readLn
        putStrLn $ "Enter all Matrix elements as one list, space separated, moving by row " ++ name ++ ": "
        elems <- fmap (map read .  words) getLine
        return $ makeMatrix norows nocols elems
    
    carryAction :: (Matrix -> Matrix -> Matrix) -> String -> IO ()
    carryAction act name = do
        putStrLn $ name ++ " of Matrices"
        matA <- inputMatrix "A"
        matB <- inputMatrix "B"
        let result = matA `act` matB
        showMatrix result

    carryActionIdentity :: IO ()
    carryActionIdentity = do
        putStrLn "Identity Matrix"
        putStrLn "Enter size of Identity Element"
        size <- readLn
        let result = identityMatrix size
        showMatrix result

    carryActionScalar :: IO ()
    carryActionScalar = do
        putStrLn "Scalar Multiplication"
        putStrLn "Enter Scalar"
        scalar <- readLn
        mmatrix <- inputMatrix " "
        let result = scalarMulMatrix scalar mmatrix
        showMatrix result

    carryActionTranspose :: IO ()
    carryActionTranspose = do
        putStrLn "Transpose Matrix"
        mat <- inputMatrix " "
        let result = transposeMatrix mat
        showMatrix result