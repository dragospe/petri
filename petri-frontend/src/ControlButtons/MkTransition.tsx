type MkTransitionProps = {isActive : Boolean}

export default ({isActive} : MkTransitionProps) => {
  return (
      <div className={"mkTransitionButton " + (isActive ? "active" : "inactive")}>
        {isActive ? "T+" : "T"}
        </div>
  );
};
