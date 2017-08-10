{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Java.SyntaxClasses where

import           Data.Function        (on)
import           Language.Java.Syntax

-- | Provides functionality to access the body as a list of declarations of a class, enum and an interface.
class HasBody a l where
  getBody :: a -> [DeclNode l]

-- | Get type of TypeDeclNode
instance HasType (TypeDeclNode l) where
  getType (ClassTypeDeclNode _ ctd) = getType ctd
  getType (InterfaceTypeDeclNode _ itd) = getType itd

instance CollectTypes (TypeDeclNode l) where
  collectTypes (ClassTypeDeclNode _ ctd) = collectTypes ctd
  collectTypes (InterfaceTypeDeclNode _ itd) = collectTypes itd

-- | Get the body of TypeDecl
instance HasBody (TypeDeclNode l) l where
  getBody (ClassTypeDeclNode _ classDeclB) = getBody classDeclB
  getBody (InterfaceTypeDeclNode _ iterDecl) = getBody iterDecl

-- | Get type of ClassDecl
instance HasType (ClassDeclNode l) where
  getType (ClassDeclNode _ ctd) = getType ctd
  getType (EnumDeclNode _ itd) = getType itd
  --getType (ClassDeclNode _ _ i _ _ _ _) = withoutPackageIdentToType i
  --getType (EnumDeclNode _ _ i _ _) = withoutPackageIdentToType i

-- | Get the body of ClassDecl
instance HasBody (ClassDeclNode l) l where
  getBody (ClassDeclNode _ ctd) = getBody ctd
  getBody (EnumDeclNode _ itd) = getBody itd
  --getBody (ClassDeclNode _ _ _ _ _ _ classBodyB) = getBody classBodyB
  --getBody (EnumDeclNode _ _ _ _ enumBodyB) = getBody enumBodyB

instance CollectTypes (ClassDeclNode l) where
  collectTypes (ClassDeclNode _ ctd) = collectTypes ctd
  collectTypes (EnumDeclNode _ itd) = collectTypes itd
  --collectTypes (ClassDeclNode _ _ i _ _ types _) = withoutPackageIdentToType i : collectTypes types
  --collectTypes (EnumDeclNode _ _ i types _) = withoutPackageIdentToType i : collectTypes types

-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes (MemberDeclNode l) where
  collectTypes (FieldDeclNode _ ctd) = collectTypes ctd
  collectTypes (MethodDeclNode _ ctd) = collectTypes ctd
  collectTypes (ConstructorDeclNode _ ctd) = []
  collectTypes (MemberClassDeclNode _ ctd) = collectTypes ctd
  collectTypes (MemberClassDeclNode _ ctd) = collectTypes ctd
  collectTypes (MemberInterfaceDeclNode _ ctd) = collectTypes ctd

instance Eq l => Ord (MemberDeclNode l) where
  compare = compare `on` memToInt
    where
      memToInt FieldDeclNode{} = 1
      memToInt MethodDeclNode{} = 2
      memToInt ConstructorDeclNode{} = 3
      memToInt MemberClassDeclNode{} = 4
      memToInt MemberInterfaceDeclNode{} = 5

instance HasType (ImportDecl l) where
  getType = getTypeFromPackage . importPackage

getTypeFromPackage :: Package -> Type
getTypeFromPackage pkg = RefType $ ClassRefType $ WithPackage pkg WildcardName

-- TODO ClassTypeDecl InterfaceTypeDecl

instance HasType (ClassDecl l) where
  getType (ClassDecl _ _ i _ _ _ _) = withoutPackageIdentToType i
instance HasType (EnumDecl l) where
  getType (EnumDecl _ _ i _ _) = withoutPackageIdentToType i
-- | Get the body of ClassDecl
instance HasBody (ClassDecl l) l where
  getBody (ClassDecl _ _ _ _ _ _ classBodyB) = getBody classBodyB
instance HasBody (EnumDecl l) l where
  getBody (EnumDecl _ _ _ _ enumBodyB) = getBody enumBodyB

instance CollectTypes (ClassDecl l) where
  collectTypes (ClassDecl _ _ i _ _ types _) = withoutPackageIdentToType i : collectTypes types
instance CollectTypes (EnumDecl l) where
  collectTypes (EnumDecl _ _ i types _) = withoutPackageIdentToType i : collectTypes types

instance HasType (Extends l) where
  getType = getType . extendsClass

instance HasType (Implements l) where
  getType = getType . implementsInterface


-- | Get the body of ClassBody
instance HasBody (ClassBody l) l where
  getBody (ClassBody _ decls) = decls


-- | Get the body of EnumBody
instance HasBody (EnumBody l) l where
  getBody (EnumBody _ _ decls) = decls


-- | Get type of EnumConstant
instance HasType (EnumConstant l) where
  getType (EnumConstant _ i _ _) =  withoutPackageIdentToType i


-- | Get type of InterfaceDecl
instance HasType (InterfaceDecl l) where
  getType (InterfaceDecl _ _ _ i _ _ _) =  withoutPackageIdentToType i

instance CollectTypes (InterfaceDecl l) where
  collectTypes (InterfaceDecl _ _ _ i _ types _) = withoutPackageIdentToType i : collectTypes types

-- | Get the body of InterfaceDecl
instance HasBody (InterfaceDecl l) l where
  getBody (InterfaceDecl _ _ _ _ _ _ iterBody) = getBody iterBody


-- | Get the body of ClassDecl
instance HasBody (InterfaceBody l) l where
  getBody (InterfaceBody l memDecls) = map (MemberDeclNode l) memDecls


-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes (FieldDecl l) where
  collectTypes (FieldDecl _ _ t _) =  [t]
instance CollectTypes (MethodDecl l) where
  collectTypes (MethodDecl _ _ _ _ name _ _ _ _) =  [withoutPackageIdentToType name]
instance CollectTypes (MemberClassDecl l) where
  collectTypes (MemberClassDecl _ cd) = [getType cd]
instance CollectTypes (MemberInterfaceDecl l) where
  collectTypes (MemberInterfaceDecl _ idecl) =  [getType idecl]
