type MkPlaceProps = {isActive : Boolean}

export default ({isActive} : MkPlaceProps) => {
  return (
      <div className={"mkPlaceButton " + (isActive ? "active" : "inactive")}>
        {isActive ? "P+" : "P"}
        </div>
  );
};
