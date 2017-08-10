module Language.Java.SyntaxClasses where

import           Data.Function        (on)
import           Language.Java.Syntax

-- | Provides functionality to access the body as a list of declarations of a class, enum and an interface.
class HasBody a l where
  getBody :: a -> [Decl l]

-- | Get type of TypeDecl
instance HasType (TypeDeclNode l) where
  getType (ClassTypeDecl _ ctd) = getType ctd
  getType (InterfaceTypeDecl _ itd) = getType itd

instance CollectTypes (TypeDeclNode l) where
  collectTypes (ClassTypeDecl _ ctd) = collectTypes ctd
  collectTypes (InterfaceTypeDecl _ itd) = collectTypes itd

-- | Get the body of TypeDecl
instance HasBody (TypeDeclNode l) l where
  getBody (ClassTypeDecl _ classDeclB) = getBody classDeclB
  getBody (InterfaceTypeDecl _ iterDecl) = getBody iterDecl

-- | Get type of ClassDecl
instance HasType (ClassDeclNode l) where
  getType (ClassDecl _ _ i _ _ _ _) = withoutPackageIdentToType i
  getType (EnumDecl _ _ i _ _) = withoutPackageIdentToType i

-- | Get the body of ClassDecl
instance HasBody (ClassDeclNode l) l where
  getBody (ClassDecl _ _ _ _ _ _ classBodyB) = getBody classBodyB
  getBody (EnumDecl _ _ _ _ enumBodyB) = getBody enumBodyB

instance CollectTypes (ClassDeclNode l) where
  collectTypes (ClassDecl _ _ i _ _ types _) = withoutPackageIdentToType i : collectTypes types
  collectTypes (EnumDecl _ _ i types _) = withoutPackageIdentToType i : collectTypes types

-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes (MemberDeclNode l) where
  collectTypes (FieldDecl _ _ t _) =  [t]
  collectTypes (MethodDecl _ _ _ _ name _ _ _ _) =  [withoutPackageIdentToType name]
  collectTypes ConstructorDecl{} = []
  collectTypes (MemberClassDecl _ cd) =  [getType cd]
  collectTypes (MemberInterfaceDecl _ idecl) =  [getType idecl]

instance Eq l => Ord (MemberDeclNode l) where
  compare = compare `on` memToInt
    where
      memToInt FieldDecl{} = 1
      memToInt MethodDecl{} = 2
      memToInt ConstructorDecl{} = 3
      memToInt MemberClassDecl{} = 4
      memToInt MemberInterfaceDecl{} = 5

instance HasType (ImportDecl l) where
  getType = getTypeFromPackage . importPackage

getTypeFromPackage :: Package -> Type
getTypeFromPackage pkg = RefType $ ClassRefType $ WithPackage pkg WildcardName

-- TODO ClassTypeDecl InterfaceTypeDecl
-- | Get type of TypeDecl
instance HasType (TypeDecl l) where
  getType (ClassTypeDecl _ ctd) = getType ctd
  getType (InterfaceTypeDecl _ itd) = getType itd

instance CollectTypes (TypeDecl l) where
  collectTypes (ClassTypeDecl _ ctd) = collectTypes ctd
  collectTypes (InterfaceTypeDecl _ itd) = collectTypes itd

-- | Get the body of TypeDecl
instance HasBody (TypeDecl l) l where
  getBody (ClassTypeDecl _ classDeclB) = getBody classDeclB
  getBody (InterfaceTypeDecl _ iterDecl) = getBody iterDecl

-- | Get type of ClassDecl
instance HasType (ClassDecl l) where
  getType (ClassDecl _ _ i _ _ _ _) = withoutPackageIdentToType i
  getType (EnumDecl _ _ i _ _) = withoutPackageIdentToType i

-- | Get the body of ClassDecl
instance HasBody (ClassDecl l) l where
  getBody (ClassDecl _ _ _ _ _ _ classBodyB) = getBody classBodyB
  getBody (EnumDecl _ _ _ _ enumBodyB) = getBody enumBodyB

instance CollectTypes (ClassDecl l) where
  collectTypes (ClassDecl _ _ i _ _ types _) = withoutPackageIdentToType i : collectTypes types
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
  getBody (InterfaceBody l memDecls) = map (MemberDecl l) memDecls


-- | Get type of MemberDecl if it is a MethodDecl (our solution to handeling the Maybe)
instance CollectTypes (MemberDecl l) where
  collectTypes (FieldDecl _ _ t _) =  [t]
  collectTypes (MethodDecl _ _ _ _ name _ _ _ _) =  [withoutPackageIdentToType name]
  collectTypes ConstructorDecl{} = []
  collectTypes (MemberClassDecl _ cd) =  [getType cd]
  collectTypes (MemberInterfaceDecl _ idecl) =  [getType idecl]

instance Eq l => Ord (MemberDecl l) where
  compare = compare `on` memToInt
    where
      memToInt FieldDecl{} = 1
      memToInt MethodDecl{} = 2
      memToInt ConstructorDecl{} = 3
      memToInt MemberClassDecl{} = 4
      memToInt MemberInterfaceDecl{} = 5
